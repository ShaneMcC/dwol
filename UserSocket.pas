{*
 * UserSocket - Maintains the sockets used by clients
 * Copyright (C) 2005 Shane "Dataforce" Mc Cormack
 * For conditions of distribution and use, see copyright notice in license.txt
 *
 * SVN: $Id$
 *}
unit UserSocket;

{$I defines.inc}

interface

uses SysUtils, Classes, {$ifdef win32}WSocket, Wcore{$else}lsocket, lcore{$endif},
     {$IFNDEF UseCore}extctrls, {$ENDIF}fastMD5, nixtime, ipstrings, DebugList,
     svn, datafile, GameInfo;

const
{$I consts.inc}

type

  {$IFDEF UseCore}TTimer = TLTimer;{$ENDIF}

  TInfo = record
    APGAR: String;
    Username: String;
    Serial: String;
    GavePass: Boolean;
  end;

  TWOL_Socket =  class(tobject) // Clients LinkedList Type
    sock: TWSocket;               // Client Nickname
    isClosing: Boolean;
    IPInt: LongWord;
    Buffer: String;
    Timer: TTimer;
    NeedPing: Boolean;
    SkipPing: Boolean;
    ConnectedTime: longint;
    info: TInfo;
    WhichLobby: string;
    InLobby: Boolean;
    UserData: TWOL_UserData;
    FakeSocket: Boolean; // Eg, this is the Bot
    Options: array [0..1] of byte;
    CurrentChannel: String;
    ChanMode: Integer;
    SKU: Integer;

    Prev: TWOL_Socket;          // Pointer to previous client (If not first)
    Next: TWOL_Socket;          // Pointer to next client (If not last)

    procedure SessionClose(sender : tobject; error : word);
    procedure DataAvail(Sender: TObject; Error: word);
    procedure ProcessLine();
    procedure ProcessServServ(Line: String = '');
    procedure ProcessGameServ(Line: String = '');
    procedure ChannelCommands(var TokenParams: TStringList);
    procedure AdminChannelCommands(var TokenParams: TStringList);
    function CheckChannelMessage(var TokenParams: TStringList): ShortInt;
    procedure BotCommands(var TokenParams: TStringList);        
    procedure JoinGame(var Tokens: TStringList);
    procedure TimerEvent(Sender: TObject);
    procedure RejectClient(Reason:string; Use378: Boolean = True);
    procedure ListLobby();
    procedure ListGames();

    function Send(Line: String): Boolean;
  end;

var

  TheBot: TWOL_Socket;

  SocketTotal: integer;
  FirstSocket: TWOL_Socket;
  LastSocket: TWOL_Socket;

  procedure LL_InitSockets();
  function LL_AddSocket(AddFirst: Boolean = True): TWOL_Socket;
  procedure LL_DeleteSocket(Socket: TWOL_Socket);
  function LL_FindSocket(Username: String; IgnoreSock: TWOL_Socket = nil): TWOL_Socket; overload;

implementation
uses Common, Channels;

procedure LL_InitSockets();
var
  ptr: TWOL_Socket;
begin
  if FirstSocket <> nil then begin
    ptr := FirstSocket;
    while ptr <> nil do begin
      if ptr.prev <> nil then begin
        ll_deletesocket(ptr.prev);
      end;
      if ptr.next <> nil then begin
        ptr := ptr.next
      end else begin
        ll_deletesocket(ptr);
        break;
      end;
    end;
  end;

  LastSocket := nil;
  FirstSocket := nil;
  SocketTotal := 0;
end;


//----------------------------------------------------------------------------
// Add a new Socket object
// AddFirst is used to decide if it is added at the start or end
//----------------------------------------------------------------------------
function LL_AddSocket(AddFirst: Boolean = True): TWOL_Socket;
var
  NewSocket: TWOL_Socket;
begin
  NewSocket := TWOL_Socket.Create;

  if AddFirst then begin
    NewSocket.Next := FirstSocket;
    NewSocket.Prev := nil;
    if SocketTotal = 0 then begin
       LastSocket := NewSocket;
    end else begin
       FirstSocket.Prev := NewSocket;
    end;
    FirstSocket := NewSocket;
  end else begin
    NewSocket.Prev := LastSocket;
    NewSocket.Next := nil;
      if SocketTotal = 0 then begin
        FirstSocket := NewSocket;
      end else begin
        LastSocket.Next := NewSocket;
      end;
    LastSocket := NewSocket;
  end;

  NewSocket.sock := TSocket.create(nil);
  NewSocket.Buffer := '';

  NewSocket.sock.OnDataAvailable := NewSocket.DataAvail;
  NewSocket.sock.Onsessionclosed := NewSocket.SessionClose;

  NewSocket.ConnectedTime := unixtime();
  NewSocket.IPInt := 0;
  NewSocket.Buffer := '';

  NewSocket.Timer := TTimer.create(nil);
  NewSocket.Timer.ontimer := NewSocket.TimerEvent;
  if Config.ServServ then NewSocket.Timer.interval := 10000
  else NewSocket.Timer.interval := 30000;
  NewSocket.Timer.enabled := false;

  NewSocket.NeedPing := false;
  NewSocket.SkipPing := false;
  NewSocket.isClosing := False;
  NewSocket.inLobby := False;

  Inc(SocketTotal);
  Result := NewSocket;
end;

//----------------------------------------------------------------------------
// Delete a Socket object
//----------------------------------------------------------------------------
procedure LL_DeleteSocket(Socket: TWOL_Socket);
begin
  if Socket = nil then exit;

  Socket.UserData := nil; 
  Socket.isClosing := True;
  if Socket.Sock <> nil then begin
    Socket.Sock.OnDataAvailable := nil;
    Socket.Sock.Onsessionclosed := nil;
    Socket.Sock.Release;
    Socket.Sock := nil;
  end;

  if not assigned(Socket.Prev) then begin
    // i.e., the Socket is first
    FirstSocket := Socket.Next;
  end;

  if not assigned(Socket.next) then begin
    // i.e., the Socket is last
    LastSocket := Socket.Prev;
  end;

  if Socket.Prev <> nil then begin
    Socket.Prev.Next := Socket.Next;
  end;

  if Socket.Next <> nil then begin
    Socket.Next.Prev := Socket.Prev;
  end;

//  FreeAndNil(Socket.sock);
//  FreeAndNil(Socket);

  Socket.Timer.ontimer := nil;
  Socket.Timer.enabled := false;
  Socket.Timer.Free;
  Socket.Timer := nil;

  Socket.Buffer := '';

  Socket.Free;
  Dec(SocketTotal);
end;

function LL_FindSocket(Username: String; IgnoreSock: TWOL_Socket): TWOL_Socket; overload;
var
  ptr: TWOL_Socket;
begin
  Result := nil;
  ptr := FirstSocket;
  while ptr <> nil do begin
    if lowercase(ptr.info.username) = lowercase(username) then begin
      if ptr <> IgnoreSock then begin
        Result := ptr;
        break;
      end;
    end;
    ptr := ptr.next;
  end;
end;

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------

function TWOL_Socket.Send(Line: String): Boolean;
begin
  Result := True;
  if FakeSocket then exit;
  AddDebug(DEBUG_PROC,'TWOL_Socket.Send');
  Result := SendLine(Self.Sock,Line);
end;    

procedure TWOL_Socket.SessionClose(sender : tobject; error : word);
var
  Channel: TWOL_ChannelData;
  ChannelClient: TWOL_ChannelClient;
begin
  DoWriteLN('[TWOL_Socket.SessionClose] Removing Client from channels');
  Channel := FirstChannelData;
  while Channel <> nil do begin
    ChannelClient := Channel.FindChannelClient(Info.Username);
    if ChannelClient <> nil then begin
      DoWriteLN('[TWOL_Socket.SessionClose] -> '+Channel.Name);
      Channel.SendToChannel(':'+Info.Username+'!UserName@HOST PART '+Channel.Name,Self);

      Channel.DeleteChannelClient(ChannelClient);
      if Channel.ChannelClientTotal = 0 then LL_DeleteChannelData(Channel);
    end;
    Channel := Channel.Next;
  end;
  DoWriteLN('[TWOL_Socket.SessionClose] Saving User Changes to SQL.');
  SaveUserData(Self.UserData);
  ll_DeleteUserData(Self.UserData);
  Self.UserData := nil;
  DoWriteLN('[TWOL_Socket.SessionClose] Done');
  ll_DeleteSocket(Self);
  AddDebug(DEBUG_PROC,'TWOL_Socket.SessionClose');
  DoWriteLN('[TWOL_Socket.SessionClose] Socket Closed.');
  DoWriteLN('[TWOL_Socket.SessionClose] Close Complete');
end;

procedure TWOL_Socket.DataAvail(Sender: TObject; Error: word);
var
  Len: Integer;
  LocalBuffer: Array [0..511] of char;
  Line: String;
  I: Integer;
begin
  if isClosing then exit;
  AddDebug(DEBUG_PROC,'TWOL_Socket.DataAvail');
  DoWriteLN('[TWOL_Socket.DataAvail] Got Data');
  Len := sock.Receive(@LocalBuffer, SizeOf(LocalBuffer) - 1);
  LocalBuffer[Len] := #0;
  DoWriteLN('[TWOL_Socket.DataAvail] Data Length: '+inttostr(Len));
  if Len > 0 then begin
    Line := Self.Buffer + LocalBuffer;
    Self.Buffer := '';
    for I := 1 to Length(Line) do begin
      if (Line[I] <> chr(13)) and (Line[I] <> chr(10)) then begin
        Self.Buffer := Self.Buffer + Line[I];
      end
      else begin
        if isClosing then exit;
        if Self.Buffer <> '' then begin
          DoWriteLN('[TWOL_Socket.DataAvail] Processing Buffer');
          ProcessLine();
        end;
      end;
    end;
  end;
  DoWriteLN('[TWOL_Socket.DataAvail] End');
end;

procedure TWOL_Socket.ProcessLine();
var
  Line: String;
begin
  AddDebug(DEBUG_PROC,'TWOL_Socket.ProcessLine');
  DoWriteLN('[TWOL_Socket.ProcessLine] Processing Line');
  try
    NeedPing := false;
    SkipPing := true;
    Line := Buffer;
    Buffer := '';
    if Config.ServServ then ProcessServServ(Line)
    else ProcessGameServ(Line);
  except
    on E : Exception do begin
      if Config.ServServ then DoDebugTrace(E, 'TWOL_Socket.ProcessLine:ProcessServServ')
      else DoDebugTrace(E, 'TWOL_Socket.ProcessLine:ProcessGameServ')
    end;
  end;
end;

procedure TWOL_Socket.ListGames();
var
  Channel: TWOL_ChannelData;
  TournStr: String[1];
  LockStr: String[3];
  ResStr: String[3];
  UnknownStr: String[1];  
begin
  //#dylthpil8's_game 6 0 41 0 1 2375561990 128::g16O25,2097731398,0,0,0,XMP17T6.MAP
  //#Game01's_game 1 0 33 0 1 215551422 128::g14N39,1878366581,0,0,0,ZZZZZZ~1.MPR
  //#DForce88's_game 1 0 41 0 1 3232235522 0::g12P25,2097731398,0,0,0,XDeadman.MAP
// #kingisd0a's_game 1 0 41 0 1024 413328217 128::g12O25,2097731398,0,0,0,XMP21S2.MAP
  //NAME Players N/A ListType Tourn <RESOLUTION> HostIP <Locked?>::<TOPIC - JUST SPEW WHAT CLIENT GIVES :D>
  Send(':'+Config.MyName+' 321 '+Info.Username+' Channel :Users  Name');

  Channel := FirstChannelData;
  while Channel <> nil do begin
    if (Channel.Admin and UserData.IsAdmin) or (not Channel.Admin) then begin
      if (Channel.IsGame) and (Not Channel.Hidden) then begin
        if (Channel.ChanMode = Self.ChanMode) then begin
          if Channel.IsTournament then TournStr := '1' else TournStr := '0';
          if Channel.Password <> '' then LockStr := '348' else LockStr := '128';
          ResStr := '0'; // Alternatives = 640 / 800 / 1024
          UnknownStr := '0';

          Send(':'+Config.MyName+' 326 '+Info.Username+' '+Channel.name+' '+inttostr(Channel.ChannelClientTotal)+' '+UnknownStr+' '+inttostr(Channel.ChanMode)+' '+TournStr+' '+ResStr+' '+inttostr(Channel.GameOwner.IPInt)+' '+LockStr+'::'+Channel.Topic);
        end;
      end;
    end;
    Channel := Channel.next;
  end;
  Send(':'+Config.MyName+' 323 '+Info.Username+' :End of /LIST');
end;

procedure TWOL_Socket.ListLobby();
var
  Channel: TWOL_ChannelData;
begin
  Send(':'+Config.MyName+' 321 '+Info.Username+' Channel :Users  Name');
  Channel := FirstChannelData;
  while Channel <> nil do begin
    if (Channel.Admin and UserData.IsAdmin) or (not Channel.Admin) then begin
      if (not Channel.IsGame) and (Not Channel.Hidden) then begin
        Send(':'+Config.MyName+' 327 '+Info.Username+' '+Channel.name+' '+inttostr(Channel.ChannelClientTotal)+' 0 2436');
      end;
    end;
    Channel := Channel.next;
  end;
  Send(':'+Config.MyName+' 323 '+Info.Username+' :End of /LIST');
end;


procedure TWOL_Socket.ProcessServServ(Line: String = '');
var
  Token: TStringList;
  i: integer;
  ptr: TWOL_ServerData;
begin
  Token := nil;
  try
    Split(' ',Line,Token);
    if Token.Count = 0 then exit;
    Token[0] := uppercase(Token[0]);
    DoWriteLN('[ServServ]:> '+Token[0]);
    if Token[0] = 'VERCHK' then begin
      // We should do something here really
    end
    else if Token[0] = 'CVERS' then begin
      // satisfy stupid XWIS Checker which sends data for a GameServ not a ServServ
      Send(':'+Config.MyName+' 421 UserName '+Token[0]+' :Unknown Command');
      Send(':'+Config.MyName+' -> Check Passed.');
    end
    else if Token[0] = 'LOBCOUNT' then begin
      if Token.Count < 2 then begin
        Send(':'+Config.MyName+' 461 UserName '+Token[0]+' :Not enough parameters');
        exit;
      end;
      WhichLobby := Token[1];
      Send(':'+Config.MyName+' 610 UserName 1');
    end
    else if (Token[0] = 'WHERETO') then begin
      i := 0;
      if Config.ShowOfficial then begin
        if WhichLobby = '8448' then begin
          i := 9;
          Send(':'+Config.MyName+' 605 UserName :ra2chattest.westwood.com 7001 ''0,1:USA Server'' -8 36.1083 -115.0582');
          Send(':'+Config.MyName+' 605 UserName :ra2chat.westwood.com 7001 ''2,3,4,5,6:European Server'' -8 36.1083 -115.0582');
          Send(':'+Config.MyName+' 605 UserName :ra2chattest.westwood.com 7000 ''7,8:Pacific Server'' -8 36.1083 -115.0582');
        end
        else if WhichLobby = '10496' then begin
          i := 11;
          Send(':'+Config.MyName+' 605 UserName :ra2mission2.westwood.com 7000 ''0,1:USA Server'' -8 36.1083 -115.0582');
          Send(':'+Config.MyName+' 605 UserName :ra2mission1.westwood.com 7001 ''2,3,4,5,6,7,8:European Server'' -8 36.1083 -115.0582');
          Send(':'+Config.MyName+' 605 UserName :ra2mission1.westwood.com 7002 ''9,10:Pacific Server'' -8 36.1083 -115.0582');
        end;
      end;

      ptr := FirstServerData;
      While ptr <> nil do begin
        if ptr.ValidSKU(WhichLobby) then begin
          Send(':'+Config.MyName+' 605 UserName :'+ptr.name+' '+inttostr(ptr.port)+' '''+inttostr(i)+','+inttostr(i+1)+':'+ptr.Description+''' -8 36.1083 -115.0582');
          i := i+2;
        end;
        ptr := ptr.next;
      end;
      Send(':'+Config.MyName+' 605 UserName :irc.westwood.com 4000 ''Live chat server'' -8 36.1083 -115.0582');
      Send(':'+Config.MyName+' 608 UserName :irc.westwood.com 4006 ''Gameres Server'' -8 36.1083 -115.0582');
      Send(':'+Config.MyName+' 609 UserName :irc.westwood.com 4007 ''Ladder Server'' -8 36.1083 -115.0582');

    end
    else if (token[0] = 'QUIT') then begin
        Send(':'+Config.MyName+' 607 UserName :goodbye');
        Sock.Close;
    end
    else begin
      Send(':'+Config.MyName+' 421 UserName '+Token[0]+' :Unknown Command');
    end;
  finally
    if Token <> nil then begin
      token.free;
      token.Clear;
    end;
  end;
end;

procedure TWOL_Socket.RejectClient(Reason:string; Use378: Boolean = True);
begin
  if (Use378) then Send(':'+Config.MyName+' 378 UserName :Error: '+Reason);
  Send(':'+Config.MyName+' 375 UserName :- Welcome to Westwood Online!');
  Send(':'+Config.MyName+' 372 UserName :- Closing Connection. '+Reason+'');
  Send(':'+Config.MyName+' 376 UserName :End of /MOTD command.');
  Send('ERROR :Closing Link: UserName['+sock.GetPeerAddr+'] ('+Reason+')');
  Sock.Close;
end;

procedure TWOL_Socket.JoinGame(var Tokens: TStringList);
var
  Channel: TWOL_ChannelData;
  ChannelClient: TWOL_ChannelClient;
  ChannelClientCheck: TWOL_ChannelClient;
  TempStr: String;
begin
  DoWriteLN('[JOINGAME]:> '+Tokens[1]);
  Channel := ll_findchanneldata(Tokens[1]);

   if Tokens.count = 9 then Tokens.Add(''); // No Pasword given, so add an empty one
   //JOINGAME #user's_game unknown numberOfPlayers gameType unknown1 unknown2 gameIsTournament unknown password

  if Channel = nil then begin
    // New Game
    if lowercase(Tokens[1]) <> '#'+lowercase(Info.Username)+'''s_game' then begin
      // Not the right name for the game.
      SendLine(Sock,':'+Config.MyName+' 479 '+Info.Username+' '+Tokens[1]+' :Sorry, cannot join channel.');
      exit;
    end;
    if Tokens.count < 9 then begin
      SendLine(Sock,':'+Config.MyName+' 461 '+Info.Username+' '+Tokens[0]+' :Not enough parameters');
      exit;
    end;

    Channel := ll_addchanneldata();
    Channel.ChanMode := Self.ChanMode; // What Game?    
    Channel.Hidden := False;  // Game can be seen by people
    Channel.Locked := false;  // Game can be started without an admin
    Channel.Name := Tokens[1];  // Name
    Channel.Password := Tokens[9];  // Password
    Channel.IsGame := true; // Lobby or game?
    Channel.GameOptions := strtoint(Tokens[8]); // Options (No idea)
    Channel.GameOwner := Self; // Owner of game. (Only owner can start)
    Channel.IsTournament := false; // Not a Tournament
    Channel.unlockrequested := false; // User has not asked for game to be unlocked
    Channel.Topic := ''; // No Topic yet.
    if Tokens[7] = '1' then Channel.IsTournament := true;  // Game is tournament

    ChannelClient := Channel.AddChannelClient;  // Add Client to channel
    ChannelClient.User := Self;
    ChannelClient.Mode[0] := '1'; // Client is opped because they are the host

//    Channel.JoinInfo := Tokens[2]+' '+Tokens[3]+' '+Tokens[4]+' '+Tokens[6]+' '+Tokens[7]+' '+inttostr(IPInt)+' '+inttostr(UserData.Clan)+' :'+Channel.Name;
    Channel.JoinInfo := Tokens[2]+' '+Tokens[3]+' '+Tokens[4]+' '+Tokens[6]+' ';
  end
  else begin
    // Existing Game
    if (not Channel.IsGame) then begin
      SendLine(Sock,':'+Config.MyName+' 479 '+Info.Username+' '+Tokens[1]+' :Sorry, cannot join channel.');
      exit;
    end;

    ChannelClient := Channel.AddChannelClient;
    ChannelClient.User := Self;
  end;

  Send(':'+Config.MyName+' 332 '+Info.Username+' '+Tokens[1]+' :'+Channel.Topic);

  ChannelClientCheck := Channel.FFirstChannelClient;
  while ChannelClientCheck <> nil do begin
    if ChannelClientCheck.User.sock <> nil then begin
      if Channel.IsTournament then TempStr := '1' else TempStr := '0';
      ChannelClientCheck.User.Send(':'+Info.Username+'!UserName@HOST JOINGAME '+Channel.JoinInfo+inttostr(UserData.Clan)+' '+inttostr(IPInt)+' '+TempStr+' :'+Channel.Name);
    end;

    TempStr := '';
    if ChannelClientCheck.Mode[0] = '1' then begin
      // Opped
      TempStr := '@';
    end
    else if ChannelClientCheck.Mode[1] = '1' then begin
      // Voiced
      TempStr := '+';
    end;
    Send(':'+Config.MyName+' 353 '+Info.Username+' * '+Tokens[1]+' :'+TempStr+ChannelClientCheck.User.info.Username+','+inttostr(ChannelClientCheck.User.Userdata.Clan)+','+inttostr(ChannelClientCheck.User.IPInt)+' ');
    ChannelClientCheck := ChannelClientCheck.Next;
  end;
  Send(':'+Config.MyName+' 366 '+Info.Username+' '+Tokens[1]+' :End of /NAMES list.');
  Self.InLobby := (not Channel.IsGame);
  CurrentChannel := Channel.Name;  
end;

// This checks if the message can be sent out to the Server
// -1 = Send nowhere, Dont Process
// 0 = Send nowhere, Process
// 1 = Send to all, Process
function TWOL_Socket.CheckChannelMessage(var TokenParams: TStringList): ShortInt;
var
  sMessage: String;
begin
  sMessage := TokenParams[TokenParams.count-1];
  if sMessage[1] = '/' then Result := 0
  else Result := 1;
end;

// Process Public Trigger Commands (/me !info etc)
procedure TWOL_Socket.ChannelCommands(var TokenParams: TStringList);
var
  sMessage: String;
  TempStr: String;
  Token: TStringList;
  Clan: rClanInfo;
  UD: TWOL_Socket;
begin
  sMessage := TokenParams[TokenParams.count-1];
  Token := nil;
  Try
    Split(' ',sMessage,Token);
    if lowercase(Token[0]) = '/help' then begin
      if Token.Count > 1 then begin
        if lowercase(Token[1]) = 'help' then begin
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'-------');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'dWOL Help -> '+Token[1]);
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'-------');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'This command gives you help.');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'-------');
        end
//        else if lowercase(Token[1]) = 'updateserial' then begin
//        end
        else if lowercase(Token[1]) = 'clan' then begin
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'-------');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'dWOL Help -> '+Token[1]);
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'-------');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'The clan command lets you manage clans.');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+' ');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'**Creation**');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'/clan create clan');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'Will create a new closed clan with the tag "clan" with no join password');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+' ');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'**Join**');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'/clan join clan password');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'Will try to join the clan with the tag "clan" using a password of "password"');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+' ');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'**Kick**');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'/clan kick nickname');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'Will allow the clan owner to kick nickname from the clan');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+' ');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'**Leave**');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'/clan leave');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'Will leave your clan (if you are owner, this will disband the clan)');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+' ');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'**Description**');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'/clan name Test Clan');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'This will allow the clan owner to set the "real name" of the clan (Displayed in status bar)');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+' ');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'**Info**');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'/clan Info <clan>');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'Will give info on the specified clan. If clan is not specified, will give info on your clan.');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+' ');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'**Open/Close**');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'/clan open OR /clan close');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'This allows the Clan owner to choose to allow members to join the clan or not.');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+' ');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'**Change Password**');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'/clan setpass oldpass newpass newpass');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'This allows the Clan owner change the join password.');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'You can remove the join password by doing:');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'/clan clearpass oldpass');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'-------');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'You may need to scroll up to see all the help.');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'-------');
        end
        else if lowercase(Token[1]) = 'admin' then begin
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'-------');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'dWOL Help -> '+Token[1]);
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'-------');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'This command will alert the admins that you require assitance.');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'Abuse may cause you to be banned.');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'-------');
        end
        else if lowercase(Token[1]) = 'news' then begin
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'-------');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'dWOL Help -> '+Token[1]);
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'-------');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'This command will tell you the latest news from the admins.');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'-------');
        end
        else begin
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'Sorry I do not know about "'+Token[1]+'"');
        end;
      end
      else begin
        Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'-------');
        Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'dWOL Help');
        Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'-------');
        Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'/help       Command Help');
        Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'/clan       Clan Commands');
        Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'/admin      Call an admin');
        Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'/news       Get News');
        Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'-------');
        Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'For more information on a specific command, type /help command');
        Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'Example: /help clan');
        Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'-------');
        Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'You may need to scroll up to see all the help.');
        Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'-------');
      end;
    end
    else if lowercase(Token[0]) = '/clan' then begin
      if Token.Count > 1 then begin
        if lowercase(Token[1]) = 'create' then begin
          if Token.Count > 2 then begin
            If (UserData.InClan) then begin
              Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** You are already in a clan. You must leave your current clan before you join a new one.');
            end
            else begin
              TempStr := Token[2];
              Clan := GetClanInfo(GetClanID(TempStr));
              if Clan.ID <> -1 then begin
                Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** There is already a clan called "'+TempStr+'"');
              end
              else begin
                if Length(TempStr) > 10 then begin
                  Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Clan tag can only be a maximum of 10 Characters');                
                end
                else begin
                  Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Creating Clan "'+TempStr+'"...');
                  UserData.InClan := True;
                  Clan.Name := TempStr;
                  Clan.Tag := TempStr;
                  Clan.Owner := Info.Username;
                  Clan.Time := Unixtime();
                  Clan.Open := False;
                  Clan.ID := -2;
                  SetClanInfo(Clan);
                  if Clan.ID > 0 then begin
                    UserData.Clan := Clan.ID;
                    Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Clan has been created.');
                    Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** You will now be disconnected to finish the clan creation.');
                    Sock.Close;
                  end
                  else begin
                    Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Clan creation failed.');
                  end;
                end;
              end;
            end;
          end
          else begin
            Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Not enough parameters.');
            Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** See /help clan for more information');
          end;
        end
        else if lowercase(Token[1]) = 'join' then begin
          if Token.Count > 2 then begin
            if Token.Count > 3 then TempStr := Token[3]
            else TempStr := '';
            Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Attempting to join clan "'+Token[2]+'"');
            Clan := GetClanInfo(GetClanID(Token[2]));
            if Clan.ID <> -1 then begin
              if UserData.InClan then begin
                if Clan.ID = UserData.Clan then Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** You are already in that clan')
                else Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** You can not join a new clan untill you leave your current one.');
              end
              else begin
                if not Clan.Open then begin
                  Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** That clan is not accepting new members.');
                end
                else begin
                  if TempStr = Clan.Password then begin
                    Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Password correct. You are now a member of "'+Token[2]+'"');
                    Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** You will now be disconnected to complete the join process');
                    UserData.Clan := Clan.ID;
                    Sock.Close;
                  end
                  else begin
                    Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Password incorrect.');
                  end;
                end;
              end;
            end
            else begin
              Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** There is no clan called "'+Token[2]+'"');            
            end;
          end
          else begin
            Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Not enough parameters.');
            Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** See /help clan for more information');
          end;
        end
        else if lowercase(Token[1]) = 'kick' then begin
          if Token.Count > 2 then begin
            if UserData.InClan then begin
              Clan := GetClanInfo(UserData.Clan);
              if Lowercase(Clan.Owner) = lowercase(Info.Username) then begin
                if Lowercase(Token[2]) = lowercase(Info.Username) then begin
                  Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** To remove yourself from the clan (and disband it) use /clan leave');
                end
                else begin
                  UD := ll_findsocket(Token[2]);
                  if UD = nil then begin
                    if GetUserClan(Token[2]) <> Clan.ID then begin
                      Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** User is not in this clan');
                    end
                    else begin
                      Config.SQLDB.Query('UPDATE Users SET `Clan`=0 WHERE lower(name)='''+Config.SQLDB.Escape(Token[2])+'''');
                      Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** User Removed.');
                    end;
                  end
                  else begin
                    if UD.UserData.Clan = Clan.ID then begin
                      UD.UserData.Clan := 0;
                      UD.Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** You have been kicked out of your clan.');
                      UD.Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** You will now be disconnected.');
                      UD.Sock.Close;
                      Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** User Removed.');
                    end
                    else begin
                      Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** User is not in this clan');
                    end;
                  end;
                end;
              end
              else begin
                Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** You are not the clan owner');
              end;
            end
            else begin
              Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** You are not in a clan.');
            end;
          end
          else begin
            Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Not enough parameters.');
            Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** See /help clan for more information');
          end;
        end
        else if lowercase(Token[1]) = 'leave' then begin
          if UserData.InClan then begin
            Clan := GetClanInfo(UserData.Clan);
            TempStr := md5str(Clan.Tag);
            TempStr := Copy(TempStr,1,8);
            TempStr := MakeAPGAR(TempStr);
            if Token.Count > 2 then begin
              if Token[2] <> TempStr then begin
                Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Code incorrect.');              
              end
              else begin
                Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Code correct. You have now left the clan.');
                if Lowercase(Clan.Owner) = lowercase(Info.Username) then begin
                  Config.SQLDB.Query('DELETE FROM Clans Where `ID`='+inttostr(Clan.ID));
                  UD := FirstSocket;
                  while UD <> nil do begin
                    if UD <> Self then begin
                      if UD.UserData.Clan = Clan.ID then begin
                        UD.Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Your clan has been disbanded, you will now be disconnected to remove you from the clan.');
                        UD.UserData.Clan := 0;
                        UD.sock.Close;
                      end;
                    end;
                    UD := UD.Next;
                  end;
                  Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Your Clan has been disbanded.');                  
                end;
                UserData.Clan := 0;
                Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** You will now be disconnected to complete the operation.');
                Sock.Close;
              end;
            end
            else begin
              Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** To prevent accidental leaving of the clan. You must use the following command:');
              Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** /clan leave '+TempStr);
            end;
          end
          else begin
            Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** You are not in a clan.');          
          end;
        end
        else if lowercase(Token[1]) = 'name' then begin
          if Token.Count > 2 then begin
            if UserData.InClan then begin
              Clan := GetClanInfo(UserData.Clan);
              if Lowercase(Clan.Owner) = lowercase(Info.Username) then begin
                Clan.Name := Join(' ',Token,2,-1);
                SetClanInfo(Clan);
                Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Clan Description Changed');
                Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Clan Description is now: '+Clan.Name);
              end
              else begin
                Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** You are not the clan owner');
              end;
            end
            else begin
              Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** You are not in a clan.');
            end;
          end
          else begin
            Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Not enough parameters.');
            Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** See /help clan for more information');
          end;
        end
        else if lowercase(Token[1]) = 'info' then begin
          if Token.Count > 2 then begin
            Clan := GetClanInfo(GetClanID(Token[2]));
            if Clan.ID = -1 then begin
              Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Clan does not exist.');
              exit;
            end;
          end
          else begin
            Clan := GetClanInfo(UserData.Clan);
            if Clan.ID = -1 then begin
              Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** You are not in a clan.');
              exit;
            end;
          end;
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+' Clan Information ');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'******************');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'Name: '+Clan.Tag+' [ID: '+inttostr(Clan.ID)+']');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'Description: '+Clan.Name);
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'Owner: '+Clan.Owner);
          if Clan.Website <> '' then Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'Website: '+Clan.Website);
          if Clan.Open then begin
            Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'Accepting Members: Yes');
            if Clan.Password = '' then Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'Password Required: No')
            else begin
              Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'Password Required: Yes');
              if Clan.ID = UserData.Clan then Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'Current Password: '+Clan.Password);
            end;
          end
          else Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'Accepting Members: No');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'Clan Rank: '+inttostr(Clan.Rank));
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'Creation Date: '+FormatDateTime('dd mmm yyyy/hh:nn:ss',UnixToDateTime(Clan.Time)));
        end
        else if lowercase(Token[1]) = 'open' then begin
          if UserData.InClan then begin
            Clan := GetClanInfo(UserData.Clan);
            if Lowercase(Clan.Owner) = lowercase(Info.Username) then begin
              Clan.Open := True;
              SetClanInfo(Clan);
              Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Clan is now open for members');
              Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Join password is: '+Clan.Password);
            end
            else begin
              Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** You are not the clan owner');
            end;
          end
          else begin
            Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** You are not in a clan.');
          end;
        end
        else if lowercase(Token[1]) = 'close' then begin
          if UserData.InClan then begin
            Clan := GetClanInfo(UserData.Clan);
            if Lowercase(Clan.Owner) = lowercase(Info.Username) then begin
              Clan.Open := False;
              SetClanInfo(Clan);              
              Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Clan is now not accpeting new members');
            end
            else begin
              Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** You are not the clan owner');
            end;
          end
          else begin
            Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** You are not in a clan.');
          end;
        end
        else if lowercase(Token[1]) = 'clearpass' then begin
          if UserData.InClan then begin
            Clan := GetClanInfo(UserData.Clan);
            if Lowercase(Clan.Owner) = lowercase(Info.Username) then begin
              if Token.Count > 2 then begin
                if Token[2] = Clan.Password then begin
                  Clan.Password := '';
                  SetClanInfo(Clan);
                  Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Join password changed to: '+Clan.Password);
                end
                else begin
                  Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Old password not correct.');
                end;
              end
              else begin
                Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Not enough parameters.');
                Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** See /help clan for more information');
              end;
            end
            else begin
              Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** You are not the clan owner');
            end;
          end
          else begin
            Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** You are not in a clan.');
          end;
        end
        else if lowercase(Token[1]) = 'setpass' then begin
          if Token.Count > 3 then begin
            if UserData.InClan then begin
              Clan := GetClanInfo(UserData.Clan);
              if Lowercase(Clan.Owner) = lowercase(Info.Username) then begin
                if Clan.password = '' then begin
                  Token.add(Token[3]); // Make Token[4];
                  Token[3] := Token[2];
                  Token[2] := '';
                end
                else if Token.Count = 4 then begin
                  Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Not enough parameters.');
                  Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** See /help clan for more information');
                end;
                if Token[2] <> Clan.Password then begin
                  Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Old password is incorrect');
                end
                else begin
                  if Token[3] <> Token[4] then begin
                    Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** New Passwords do not match');
                  end
                  else begin
                    Clan.Password := Token[3];
                    SetClanInfo(Clan);
                    Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Join password changed to: '+Clan.Password);
                  end;
                end
              end
              else begin
                Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** You are not the clan owner');
              end;
            end
            else begin
              Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** You are not in a clan.');
            end;
          end
          else begin
            Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Not enough parameters.');
            Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** See /help clan for more information');
          end;
        end
        else begin
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Unknown Option');
          Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** See /help clan for more information');
        end;
      end
      else begin
        Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Unknown Option');
        Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** See /help clan for more information');
      end;
    end
    else if lowercase(Token[0]) = '/admin' then begin
      Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Admin Command Disabled');
    end
    else if lowercase(Token[0]) = '/news' then begin
      Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** News Command Disabled');
    end
    else if Token[0][1] = '/' then begin
      Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Unknown Command');    
    end;


  finally
    if Token <> nil then begin
      Token.Clear;  
      Token.Free;
    end;
  end;
end;

// Process Admin Commands (!unlock etc)
procedure TWOL_Socket.AdminChannelCommands(var TokenParams: TStringList);
var
  sMessage: String;
begin
  ChannelCommands(TokenParams);
  sMessage := TokenParams[TokenParams.count-1];
end;

// Process Commands to the bot (ie changepass?)
procedure TWOL_Socket.BotCommands(var TokenParams: TStringList);
var
  sMessage: String;
begin
  sMessage := TokenParams[TokenParams.count-1];
end;

procedure TWOL_Socket.ProcessGameServ(Line: String = '');
var
  sAddr: String;
  Token: TStringList;
  I: integer;
  ptr: TWOL_Socket;
  Words: TStringList;                         
  TokenParams: TStringList;
  TokenParam: String;
  TempUserData: TWOL_UserData;
  Channel: TWOL_ChannelData;
  ChannelClient: TWOL_ChannelClient;
  TempStr: String;
  MessageType: ShortInt;
  ClanDetails: rClanInfo;
begin
  TokenParams := nil;
  Token := nil;
  try
    if (Line = '') then exit;
    sAddr := sock.getpeeraddr();
    DoWriteLN('['+sAddr+' IN ]:> '+Line);

    Split(' ',Line, Token);
    if Token.Count = 0 then exit;
    TokenParams := GetParams(Line);
    TokenParam := TokenParams[TokenParams.count-1];

    Token[0] := uppercase(Token[0]);
    DoWriteLN('[GameServ]:> '+Token[0]);
    if (token[0] = 'CVERS') then begin
      if Token.Count <> 3 then begin
          RejectClient('Invalid Parameter(s)');
      end
      else if not ValidSKU(Token[2]) then begin
          RejectClient('Wrong Game.');
      end
      else begin
        Self.SKU := strtoint(Token[2]);
      end;
    end
    else if (token[0] = 'VERCHK') then begin
      Send(':'+Config.MyName+' 379 UserName :none none none 1 1000 NONREQ');
    end
    else if (token[0] = 'PASS') then begin
      Info.GavePass := True;
      if Token.Count <> 2 then begin
          RejectClient('Invalid Parameter(s)');
      end
      else if lowercase(Token[1]) <> 'supersecret' then begin
          RejectClient('Invalid Parameter(s)');
      end;
    end
    else if (token[0] = 'NICK') then begin
      if Token.Count <> 2 then begin
          RejectClient('Invalid Parameter(s)');
      end
      else begin
        Info.Username := Token[1];
        IPInt := ipstrtoint(Sock.GetPeerAddr);
      end;
    end
    else if (token[0] = 'APGAR') then begin
      if Token.Count < 2 then begin
          RejectClient('Invalid Parameter(s)');
      end
      else begin
      // Check pass
//        Send(':'+Config.MyName+' 378 UserName :Closing Connection. '+Reason+'');
        Info.Apgar := Token[1];
      end
    end
    else if (token[0] = 'SERIAL') then begin
      if Token.Count <> 2 then begin
          RejectClient('Invalid Parameter(s)');
      end
      else begin
      // Check for banned serial
//        Send(':'+Config.MyName+' 378 UserName :Closing Connection. '+Reason+'');
        Info.Serial := Token[1];
      end
    end
    else if (token[0] = 'USER') then begin
      {if IsBanned(Client) then begin
          RejectClient('You have been banned. Contact '+Config.Contact+' for details.');
      end
      else} if Info.Username = '' then begin
          RejectClient('No Login specified.');
      end
      else begin
        if ll_finduserdata(Info.Username) <> nil then begin
          RejectClient('User already connected', False);
          exit;
        end
        else begin
          UserData := LoadUserData(Info.Username);
          if not UserData.IsNew then begin
            // User Exists \o/
            if UserData.Password <> Info.APGAR then begin
              RejectClient('Incorrect Password');
              exit;
            end;
          end
          else begin
            // New User \o/
            UserData.Password := Info.Apgar;
            UserData.Serial := MD5Str(Info.Serial);
            if GetSerialCount(UserData.Serial) > 2 then begin
              UserData.NoSave := True;            
              RejectClient('This Serial has been used too many times. Each Serial may only have a maximum of 3 accounts associated with it.');
              exit;
            end
            else begin
              SaveUserData(UserData);
            end;
          end;

          Send(':'+Config.MyName+' 001 '+Info.Username+' :Welcome to Westwood Online!');
          Send(':'+Config.MyName+' 375 '+Info.Username+' :- '+Config.MyName+' Message of the Day -');
//          if Info.Serial <> UserData.Serial then begin
//            Send(':'+Config.MyName+' 372 '+Info.Username+' :- ------------------');
//            Send(':'+Config.MyName+' 372 '+Info.Username+' :- Serial Number differs from previous known serial.');
//            Send(':'+Config.MyName+' 372 '+Info.Username+' :- If your serial has changed you should use /UpdateSerial to update the database');
//            Send(':'+Config.MyName+' 372 '+Info.Username+' :- This will prevent you getting banned based on actions of users of your old serial');
//            Send(':'+Config.MyName+' 372 '+Info.Username+' :- ------------------');
//          end;
          Send(':'+Config.MyName+' 372 '+Info.Username+' :- Running dWOL');
          Send(':'+Config.MyName+' 372 '+Info.Username+' :- Local Date and time: '+DateTimeToStr(UnixToDateTime(UnixTime)));
          Send(':'+Config.MyName+' 372 '+Info.Username+' :- Started At: '+DateTimeToStr(UnixToDateTime(Config.StartTime)));
          Send(':'+Config.MyName+' 372 '+Info.Username+' :- ');
          Words := nil; //ReadToList('./MOTD.txt');
          Split(#10,Config.MOTD,Words);
          for I := 0 to Words.Count-1 do begin
            Send(':'+Config.MyName+' 372 '+Info.Username+' :- '+Words[I]);
          end;
          Words.Clear;
          Words.free;
          Send(':'+Config.MyName+' 372 '+Info.Username+' :- *** This is not an official Westwood Server ***');
          if UserData.isadmin then begin
            Send(':'+Config.MyName+' 372 '+Info.Username+' :- --[Administration Info]--------');
            Send(':'+Config.MyName+' 372 '+Info.Username+' :- Known Users   : '+inttostr(GetUsersCount()));
            Send(':'+Config.MyName+' 372 '+Info.Username+' :- Connections   : '+inttostr(SocketTotal));
            Send(':'+Config.MyName+' 372 '+Info.Username+' :- --[/Administration Info]-------');
          end;
          if Config.CanChat then Send(':'+Config.MyName+' 372 '+Info.Username+' :- Chat is enabled.')
          else Send(':'+Config.MyName+' 372 '+Info.Username+' :- Chat is disabled.');
          Send(':'+Config.MyName+' 376 '+Info.Username+' :End of /MOTD command.');
        end;
      end;
    end
    else if (token[0] = 'PONG') then begin
    end
    else if (token[0] = 'SETLOCALE') then begin
      if Token.Count = 2 then begin
        Send(':'+Config.MyName+' 310 '+Info.Username+' '+Token[1]);
        UserData.Locale := strtoint(Token[1]);
      end
      else begin
        Send(':'+Config.MyName+' 310 '+Info.Username+' '+inttostr(UserData.Locale));
      end;
    end
    else if (token[0] = 'SETCODEPAGE') then begin
      if Token.Count = 2 then begin
        Send(':'+Config.MyName+' 329 '+Info.Username+' '+Token[1]);
        UserData.CodePage := strtoint(Token[1]);
      end
      else begin
        Send(':'+Config.MyName+' 329 '+Info.Username+' '+inttostr(UserData.CodePage));
      end;
    end
    else if (token[0] = 'GETLOCALE') then begin
      if Token.Count < 2 then begin
        Send(':'+Config.MyName+' 461 '+Info.Username+' '+Token[0]+' :Not enough parameters');
        exit;
      end;
      for I := 1 to Token.Count-1 do begin
        TempUserData := ll_finduserdata(Token[I]);
        if TempUserData <> nil then begin
          Send(':'+Config.MyName+' 309 '+Info.Username+' '+Token[I]+'`'+inttostr(TempUserData.Locale));
        end
        else begin
          Send(':'+Config.MyName+' 309 '+Info.Username);
        end;
      end;
    end
    else if (token[0] = 'GETCODEPAGE') then begin
      if Token.Count < 2 then begin
        Send(':'+Config.MyName+' 461 '+Info.Username+' '+Token[0]+' :Not enough parameters');
        exit;
      end;
      for I := 1 to Token.Count-1 do begin
        TempUserData := ll_finduserdata(Token[I]);
        if TempUserData <> nil then begin
          Send(':'+Config.MyName+' 328 '+Info.Username+' '+Token[I]+'`'+inttostr(TempUserData.CodePage));
        end
        else begin
          Send(':'+Config.MyName+' 328 '+Info.Username);
        end;
      end;
    end
    else if (token[0] = 'SETOPT') then begin
      if Token.Count < 2 then begin
        Send(':'+Config.MyName+' 461 '+Info.Username+' '+Token[0]+' :Not enough parameters');
        exit;
      end;
      Words := nil;
      Split(',',Token[1],Words);
      Options[0] :=strtoint(Words[0]);    //  Find. Off=16 / On=17
      Options[1] :=strtoint(Words[1]);    //  Page. Off=32 / On=33
      Words.Clear;
      Words.Free;
    end
    else if (token[0] = 'SQUADINFO') then begin
      if Token.count < 2 then begin
        Send(':'+Config.MyName+' 461 '+Info.Username+' '+Token[0]+' :Not enough parameters');
        exit;
      end;
      if Token[1] = '0' then ClanDetails := GetClanInfo(UserData.Clan)
      else ClanDetails := GetClanInfo(StrToInt(Token[1]));
      if ClanDetails.ID > 0 then begin
        TempStr := inttostr(ClanDetails.ID)+'`'+ClanDetails.Name+'`'+ClanDetails.Tag+'`'+inttostr(ClanDetails.Rank)+'`0`1`0`0`0';
        Send(':'+Config.MyName+' 358 '+Info.Username+' '+TempStr+'`0`0`0`0`x`x`x');
      end
      else begin
        Send(':'+Config.MyName+' 439 '+Info.Username+' :ID doesn''t exist.');
      end;
    end
    else if (token[0] = 'GETBUDDY') then begin
      Send(':'+Config.MyName+' 333 '+Info.Username+' ');
      ProcessGameServ('LIST 0 0');
    end
    else if (token[0] = 'ADDBUDDY') then begin
      Send(':'+Config.MyName+' 334 '+Info.Username+' '+Token[1]);
    end
    else if (token[0] = 'DELBUDDY') then begin
      Send(':'+Config.MyName+' 335 '+Info.Username+' '+Token[1]);
    end
    else if (token[0] = 'LIST') then begin
      if Token.Count < 2 then begin
        Send(':'+Config.MyName+' 461 '+Info.Username+' '+Token[0]+' :Not enough parameters');
        exit;
      end;
      Self.ChanMode := strtoint(Token[2]);
      if ValidChanMode(Token[1]) then begin
        ListGames();
      end
      else if Token[1] = '0' then begin
        ListLobby();
      end;
    end
    else if (token[0] = 'JOIN') then begin
      if Token.count < 2 then begin
        Send(':'+Config.MyName+' 461 '+Info.Username+' '+Token[0]+' :Not enough parameters');
        exit;
      end;
      if Token.count = 2 then Token.Add(''); // Add an empty KEY for checking
      Channel := ll_findchanneldata(Token[1]);
      if Channel = nil then exit;
      if ((Channel.Password <> Token[2]) and (not UserData.IsAdmin)) or (Channel.Admin and not UserData.IsAdmin) then begin
        Send(':'+Config.MyName+' 475 '+Info.Username+' '+Token[0]+' :Sorry, cannot join channel.');
        exit;
      end;
      ChannelClient := Channel.AddChannelClient;
      ChannelClient.User := Self;
//  Removed dur to not being able to +o the user for others to see.
//      if Self.UserData.IsAdmin then ChannelClient.Mode[0] := '1';

      Channel.SendToChannel(':'+Info.Username+'!UserName@HOST JOIN :'+inttostr(UserData.Clan)+','+inttostr(IPInt)+' '+Channel.name);
      Channel.SendToChannel(':'+Config.BotName+'!UserName@HOST MODE '+Channel.Name+' :+O '+Info.UserName, Self);
//      Channel.Send(':'+Info.Username+'!UserName@HOST JOIN :1,'+inttostr(IPInt)+' '+Channel.name);

      // Now send names list
      ChannelClient := Channel.FFirstChannelClient;
      While ChannelClient <> nil do begin
        TempStr := '';
        if ChannelClient.Mode[0] = '1' then begin
          // Opped
          TempStr := '@';
        end
        else if ChannelClient.Mode[1] = '1' then begin
          // Voiced
          TempStr := '+';
        end;
        Send(':'+Config.MyName+' 353 '+Info.Username+' * '+Token[1]+' :'+TempStr+ChannelClient.User.Info.Username+','+inttostr(ChannelClient.User.Userdata.Clan)+','+inttostr(ChannelClient.User.IPInt)+' ');
//        Send(':'+Config.MyName+' 353 '+Info.Username+' * '+Token[1]+' :'+TempStr+ChannelClient.User.Info.Username+',1,'+inttostr(ChannelClient.User.IPInt)+' ');
        ChannelClient := ChannelClient.next;
      end;
      Send(':'+Config.MyName+' 366 '+Info.Username+' '+Token[1]+' :End of /NAMES list.');
      InLobby := (not Channel.IsGame);
      CurrentChannel := Channel.Name;
      if Channel.Admin then Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'**You have Joined the Admin Channel**')
      else Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Welcome to the lobby.');
    end
    else if (token[0] = 'JOINGAME') then begin
      if Token.count < 3 then begin
        Send(':'+Config.MyName+' 461 '+Info.Username+' '+Token[0]+' :Not enough parameters');
        exit;
      end;
      JoinGame(Token);
    end
    else if (token[0] = 'PART') then begin
      if Token.count < 2 then begin
        Send(':'+Config.MyName+' 461 '+Info.Username+' '+Token[0]+' :Not enough parameters');
        exit;
      end;
      Channel := ll_findchanneldata(Token[1]);
      if Channel <> nil then begin
        Self.CurrentChannel := '';
        Channel.SendToChannel(':'+Info.Username+'!UserName@HOST PART '+Token[1]);
        if Channel.Admin then Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'**You have Left the Admin Channel**');

        ChannelClient := Channel.FindChannelClient(Info.Username);
        Channel.DeleteChannelClient(ChannelClient);
        if Channel.ChannelClientTotal = 0 then LL_DeleteChannelData(Channel);
      end;
    end
    else if Token[0] = 'TOPIC' then begin
      if Token.count < 2 then begin
        Send(':'+Config.MyName+' 461 '+Info.Username+' '+Token[0]+' :Not enough parameters');
        exit;
      end;
      Channel := ll_findchanneldata(Token[1]);
      if Channel <> nil then begin
        Channel.Topic := TokenParam;
        Channel.SendToChannel(':'+Info.Username+'!UserName@HOST TOPIC '+Token[1]+' :'+Channel.Topic);
      end;
    end
    else if Token[0] = 'GAMEOPT' then begin
      if Token.count < 2 then begin
        Send(':'+Config.MyName+' 461 '+Info.Username+' '+Token[0]+' :Not enough parameters');
        exit;
      end;
      if Token[1][1] = '#' then begin
        // Game
        Channel := ll_findchanneldata(Token[1]);
        if (Channel = nil) or (not Channel.IsGame) then exit;
        Channel.SendToChannel(':'+Info.Username+'!UserName@HOST GAMEOPT '+Token[1]+' :'+TokenParam,Self);
      end
      else begin
        // User
        Channel := FirstChannelData;
        while Channel <> Nil do begin
          if Channel.FindChannelClient(Info.Username) <> nil then begin
            if (Channel.IsGame) and (Channel.GameOwner <> nil) then begin
              Channel.SendToChannel(':'+Info.Username+'!UserName@HOST GAMEOPT '+Channel.GameOwner.Info.Username+' :'+TokenParam);
            end;
          end;
          Channel := Channel.next;
        end;
      end;
    end
    else if (token[0] = 'STARTG') then begin
      if Token.count < 3 then begin
        Send(':'+Config.MyName+' 461 '+Info.Username+' '+Token[0]+' :Not enough parameters');
        exit;
      end;
      Channel := ll_findchanneldata(Token[1]);
      if Channel = nil then exit;
      if channel.locked then begin
        Channel.SendToChannel(':'+Config.BotName+'!UserName@HOST PRIVMSG '+Channel.Name+' :'+'Sorry game creation has been locked. To unlock the game, The host must say !unlock, and wait for an admin to decide.');
        exit;
      end;
      if (Not Channel.IsGame) then begin
        Send(':'+Config.BotName+'!UserName@HOST PAGE '+Info.Username+' :'+'** Disconnected due to using non-game client.');
        Send(':'+Config.BotName+'!UserName@HOST PRIVMSG '+Info.Username+' :'+'** Disconnected due to using non-game client.');
        Send(':'+Config.BotName+'!UserName@HOST NOTICE '+Info.Username+' :'+'** Disconnected due to using non-game client.');
        Sock.Close;
        exit;
      end;
      if (Channel.GameOwner <> self) then begin
        Channel.SendToChannel(':'+Config.BotName+'!UserName@HOST PRIVMSG '+Channel.Name+' :'+'Sorry '+Info.Username+' only '+Channel.GameOwner.info.Username+' can start this game.');
        exit;
      end;
      TempStr := TempStr+Channel.GameOwner.Info.Username+' '+Channel.GameOwner.Sock.GetPeerAddr;

      ChannelClient := Channel.FFirstChannelClient;
      while ChannelClient <> nil do begin
        if ChannelClient.User <> Channel.GameOwner then begin
          TempStr := TempStr+' '+ChannelClient.User.info.Username+' '+ipinttostr(ChannelClient.User.IPInt);
        end;
        ChannelClient := ChannelClient.Next;
      end;
      i := Unixtime(); // Game Number
      Channel.SendToChannel(Info.Username+'!UserName@HOST '+Token[0]+' UserName :'+TempStr+' :'+inttostr(I)+' '+inttostr(unixtime));
    end
    else if (token[0] = 'PRIVMSG') or (token[0] = 'PAGE') then begin
      if Token.count < 2 then begin
        Send(':'+Config.MyName+' 461 '+Info.Username+' '+Token[0]+' :Not enough parameters');
        exit;
      end;
      if Token[1][1] = '#' then begin
        // PRIVMSG/PAGE To Channel
        Channel := ll_findchanneldata(Token[1]);
        // -1 = Send nowhere, Dont Process
        // 0 = Send nowhere, Process
        // 1 = Send to all, Process
        MessageType := CheckChannelMessage(TokenParams);
        if (Channel <> nil) and (MessageType <> -1) then begin
          if (Channel.Admin) then AdminChannelCommands(TokenParams)
          else ChannelCommands(TokenParams);

          if (MessageType = 1) then Channel.SendToChannel(':'+Info.Username+'!UserName@HOST '+Token[0]+' '+Channel.Name+' :'+TokenParam,Self);                    
        end;
      end
      else begin
        // PRIVMSG/PAGE To User
        if lowercase(Token[1]) = lowercase(Config.BotName) then begin
          BotCommands(TokenParams);
        end
        else begin
          ptr := ll_findsocket(Token[1]);
          if (ptr <> nil) and (ptr.Options[1] <> 32) then begin
            ptr.Send(':'+Info.Username+'!UserName@HOST '+Token[0]+' '+Token[1]+' :'+TokenParam);
            if Token[0] = 'PAGE' then Send(':'+Config.MyName+' 389 '+Info.Username+' '+Token[0]+' 1')
          end
          else begin
            if Token[0] = 'PAGE' then Send(':'+Config.MyName+' 389 '+Info.Username+' '+Token[0]+' 1')
          end;
        end;
      end;
    end
    else if (token[0] = 'FINDUSER') then begin
      if Token.count < 2 then begin
        Send(':'+Config.MyName+' 461 '+Info.Username+' '+Token[0]+' :Not enough parameters');
        exit;
      end;
      ptr := ll_findsocket(Token[1]);
      if (ptr <> nil) and (ptr.Options[0] = 17) then begin
        ptr.Send(':'+Info.Username+'!UserName@HOST 388 '+Token[1]+' 0 :'+ptr.CurrentChannel);
      end
      else begin
        ptr.Send(':'+Info.Username+'!UserName@HOST 388 '+Token[1]+' 1 ');
      end;
    end
    else if (token[0] = 'FINDUSEREX') then begin
      if Token.count < 2 then begin
        Send(':'+Config.MyName+' 461 '+Info.Username+' '+Token[0]+' :Not enough parameters');
        exit;
      end;
      ptr := ll_findsocket(Token[1]);
      if (ptr <> nil) and (ptr.Options[0] = 17) then begin
        ptr.Send(':'+Info.Username+'!UserName@HOST 398 '+Token[1]+' 0 :'+ptr.CurrentChannel+','+IntToStr(ptr.UserData.Clan));
      end
      else begin
        ptr.Send(':'+Info.Username+'!UserName@HOST 398 '+Token[1]+' 1 ');
      end;
    end
    else if (token[0] = 'QUIT') then begin
      Send(':'+Config.MyName+' 607 UserName :goodbye');
      Sock.Close;
    end
    else begin
      Send(':'+Config.MyName+' 421 UserName '+Token[0]+' :Unknown Command');
    end;
  finally
    if Token <> nil then begin
      Token.Clear;
      Token.Free;
    end;
    if TokenParams <> nil then begin
      TokenParams.Clear;
      TokenParams.Free;
    end;
  end;
end;

procedure TWOL_Socket.TimerEvent(Sender: TObject);
begin
  if FakeSocket then exit;
  AddDebug(DEBUG_PROC,'TWOL_Socket.TimerEvent');
  DoWriteLN('[TWOL_Socket.TimerEvent] Timer Started');
  if SkipPing then begin
    SkipPing := false;
    Exit;
  end;
  if Config.ServServ then begin
      Send(':'+Config.MyName+' 607 UserName :Idle Timeout');
      Sock.Close;
  end
  else begin
    if NeedPing then begin
      Send('ERROR: Closing Connection. Ping Timeout');
      Sock.Close;
    end
    else begin
      NeedPing := true;
      Send('PING :'+inttostr(unixtime));
    end;
  end;
end;

{---- Adjust global SVN revision ----}
initialization
  SVNRevision('$Id$');
end.
