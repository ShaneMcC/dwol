{*
 * wol - Main stuff
 * Copyright (C) 2005 Shane "Dataforce" Mc Cormack
 * For conditions of distribution and use, see copyright notice in license.txt
 *                                                    
 * SVN: $Id$
 *}
unit wol;

{$I defines.inc}

interface

uses Sysutils, classes, common, ControlSocket,{$ifdef win32}WCore{$else}lcore,
    {$ifdef VER1_0}linux,{$else}baseunix,unix,{$endif} lsignal{$endif},
     forking, nixtime, DebugList, svn, UserSocket, datafile, Channels, ipstrings;

type
  TEvents = class
    procedure TimerEvent(Sender: TObject);
    {$IFNDEF Win32}procedure OnSignal(sender:tobject;signal:integer);{$ENDIF}
  end;

var
  ListenSocket: TListen;
  Timer: TLTimer;
  Event: TEvents;
  {$IFNDEF Win32}Signal : tlsignal;{$ENDIF}

  procedure DoStuff();
implementation

procedure TEvents.TimerEvent(Sender: TObject);
begin
  AddDebug(DEBUG_PROC,'TEvents.TimerEvent');
  DoWriteln('[TEvents.TimerEvent] Begin');
//  DoWriteln('[TEvents.TimerEvent] Saving Database');
//  SaveDataFile();
end;

{$IFNDEF Win32}
procedure TEvents.OnSignal(sender:tobject;signal:integer);
begin
  AddDebug(DEBUG_PROC,'TEvents.OnSignal');
  DoWriteln('[TEvents.OnSignal] Got Signal');
  if signal = sigterm then begin
    DoWriteln('[TEvents.OnSignal] Got SigTerm');
//    SaveDataFile();
//    SockData := FirstSocket;
//    While SockData <> nil do begin
//      SockData.send(':-BNC!BNC@DFBNC.Server NOTICE '+SockData.Info.NickName+' :Recieved SigTerm. Shutting Down.');
//      SockData.send(':DFBNC.Server NOTICE '+SockData.Info.NickName+' :Recieved SigTerm. Shutting Down.');
//      SockData := SockData.Next;
//    end;

    EndOfMessageLoop;
    ExitMessageLoop;
    exit;
    halt;
  end;
end;
{$ENDIF}

procedure ForkAndStuff(NoFork: Boolean; PidName: String);
begin
  AddDebug(DEBUG_PROC,'ForAndStuff');
  if not NoFork then begin
    DoWriteLN('[DoStuff] Attempting to fork...');
    if DoFork() then begin
      Config.IsForked := true;
      DoWriteLN('[DoStuff] Forked successfully');
    end
    else begin
      DoWriteLN('[DoStuff] Unable to fork..');
      {$IFDEF WIN32}DoWriteLN('[DoStuff] This is normal on Current Platform.');{$ENDIF}
    end;
  end
  else begin
    DoWriteLN('[DoStuff] Not Forking.');
  end;
  WritePID(PidName);
end;

procedure DoStuff();
var
  ForceRun: Boolean;
  NoFork: Boolean;
  PidName: String;
  PidType: String;

  Client: TWOL_Socket;
  UserData: TWOL_UserData;
  Channel: TWOL_ChannelData;
  ChannelClient: TWOL_ChannelClient;
  I,J: integer;
  ClanDetails: rClanInfo;
begin
  AddDebug(DEBUG_PROC,'DoStuff');
  DoWriteln('[DoStuff] Begin');
  {$IFNDEF Win32}
    DoWriteln('[DoStuff] Setup Signal Handling');
    StartHandleSignal(sigterm);
    Signal := TLSignal.Create(nil);
    Signal.OnSignal := Event.OnSignal;
  {$ENDIF}
  ListenSocket := TListen.Create();
  if not ValidIP(Config.ListenHost) then Config.ListenHost := '0.0.0.0';
  if Config.ServServ then begin
    if (Config.ListenPort < 1) or (Config.ListenPort > 65535) then Config.ListenPort := 4005;
    PidType := 'dWOL-SS';
  end
  else begin
    if (Config.ListenPort < 1) or (Config.ListenPort > 65535) then Config.ListenPort := 7001;
    PidType := 'dWOL-GS';
  end;
  if not ListenSocket.Bind(Config.ListenHost,Config.ListenPort) then begin
    DoWriteLN('[DoStuff] Bind failed. Terminating.');
    exit;
  end;
  DoWriteln('[DoStuff] Createing Timer');
  Timer := TLTimer.create(nil);
  Timer.interval := 60000;
  Timer.ontimer := Event.TimerEvent;
  Timer.enabled := true;
  DoWriteln('[DoStuff] Timer Created');

  ForceRun := false;
  if (CLI.Exists('f') <> -1) Or (CLI.Exists('force') <> -1) then ForceRun := true;
  NoFork := false;
  if (CLI.Exists('n') <> -1) Or (CLI.Exists('nofork') <> -1) then  NoFork := true;

  PidName := 'pid.'+PidType+'.'+inttostr(Config.ListenPort);
  {$IFDEF WIN32}PidName := 'win'+Pidname;{$ENDIF}

  If CheckPID(PidName) then begin
    DoWriteln('[DoStuff] Using PIDName: '+PidName);
    ForkAndStuff(NoFork, PidName);
  end
  else begin
    DoWriteln('[DoStuff] Unable to get lock on PID File. Are we still running?');
    if not ForceRun then begin
      DoWriteln('[DoStuff] To force me to run, start with --force or -f');
      exit;
    end
    else begin
      DoWriteln('[DoStuff] Run Forced.');
      PidName := Pidname+'-'+inttostr(Unixtime());
      ForceRun := false;
      if FileExists(PidName) then begin
        while not ForceRun do begin
          PidName := 'pid.'+pIDtYPE+'.'+inttostr(Config.ListenPort)+'-'+inttostr(unixtime())+'-'+inttostr(Random(100));
          {$IFDEF WIN32}PidName := 'win'+Pidname;{$ENDIF}
          if not FileExists(PidName) then ForceRun := true;
        end;
      end;
      if CheckPID(PidName) then begin
          DoWriteln('[DoStuff] Using PIDName: '+PidName);
          ForkAndStuff(NoFork, PidName);
      end
    end;
  end;

  if Not Config.ServServ then begin

    // Add The Bot Socket
    Client := LL_AddSocket();
    Client.info.Serial := '0000000000000000000000';
    Client.info.Apgar := 'bot';
    Client.info.GavePass := True;    

    I := GetClanID('-BOT-',False);
    ClanDetails := GetClanInfo(I);
    if ClanDetails.ID = -1 then begin
      // Bot Clan hasn't been added.
      ClanDetails.ID := -2; // Create New Clan
      ClanDetails.Name := Config.BotName;
      ClanDetails.Tag := '-BOT-';
      ClanDetails.Owner := Config.BotName;
      ClanDetails.Password := '';
      ClanDetails.Open := False;
      ClanDetails.Time := Unixtime;
      ClanDetails.Website := '';
      SetClanInfo(ClanDetails);
    end;

    // Check for the bot user.
    UserData := ll_finduserdata(Config.BotName);
    if UserData = nil then begin
      UserData := ll_adduserdata();
      UserData.Name := Config.BotName;
      UserData.Time := UnixTime();
      UserData.Password := Client.Info.Apgar;
      UserData.Serial := Client.Info.Serial;
      UserData.IsAdmin := True;
      UserData.Locale := 4;
      UserData.Clan := ClanDetails.ID;
      UserData.IsBot := True;
      UserData.Time := UnixTime();
    end;
    UserData.LastConnected := UnixTime();

    // Populate the Bot Socket
    Client.UserData := UserData;    
    Client.ConnectedTime := UnixTime();
    Client.FakeSocket := True;
    Client.Info.Username := Config.BotName;
    Client.IPInt := IPStrToInt('127.0.0.1');
    Client.UserData.CodePage := 1252;
    Client.UserData.Locale := 4;
    Client.Options[0] := 17;
    Client.Options[1] := 33;
    TheBot := Client;

    // Make Playable Channels. (Channel 0 is always admin only)
    For J := 0 to Config.ChanModeList.Count-1 do begin
      For I := 0 to Config.PlayableChannels do begin
        Channel := ll_addchanneldata;
        Channel.Hidden := False;
        Channel.Name := '#Lob_'+Config.ChanModeList[J]+'_'+inttostr(I);
        Channel.Password := 'zotclot9';
        Channel.IsGame := False;
        Channel.Admin := False;
        ChannelClient := Channel.AddChannelClient;
        ChannelClient.User := TheBot;
        ChannelClient.Mode[0] := '1';
        ChannelClient.Mode[1] := '1';
      end;
      Channel := ll_FindChannelData('#Lob_'+Config.ChanModeList[J]+'_0');
      if Channel <> nil then Channel.Admin := True;
    end;
  end;

  DoWriteln('[DoStuff] Begin Message Loop');
  while not Common.EndedLoop do begin
    try
      messageloop();
    except
      on E : Exception do begin
        DoDebugTrace(E, 'DoStuff:MessageLoop');      
      end;
    end;
  end;
  EndOfMessageLoop();
  exit;
  halt;
end;

{---- Adjust global SVN revision ----}
initialization
  SVNRevision('$Id$');
end.

