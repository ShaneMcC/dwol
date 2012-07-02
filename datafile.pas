 {*
 * DataFile - Stores information about users/server.
 * Copyright (C) 2005 Shane "Dataforce" Mc Cormack
 * For conditions of distribution and use, see copyright notice in license.txt
 *
 * SVN: $Id$
 *}
unit datafile;

{$I defines.inc}

interface

uses classes, sysutils, nixtime, inifiles, DebugList, svn, SQL, {$ifdef win32}WCore{$else}lcore{$endif} ;

type

  TWOL_UserData =  class(tobject) // Clients LinkedList Type
    ID: Integer;
    Name: String;
    Password: String;
    Serial: String;
    IsAdmin: Boolean;
    IsBot: Boolean;    
    Locale: Integer;
    CodePage: Integer;
    Clan: Integer;
    Time: LongInt;
    LastConnected: LongInt;
    IsNew: Boolean;
    InClan: Boolean;
    NoSave: Boolean;

    Prev: TWOL_UserData;          // Pointer to previous client (If not first)
    Next: TWOL_UserData;          // Pointer to next client (If not last)
  end;

  TWOL_ServerData =  class(tobject) // Servers LinkedList Type
    ConfigName: String;
    Name: String;
    Host: String;
    Port: Integer;
    SKUList: TStringList;
    Description: String;

    Prev: TWOL_ServerData;          // Pointer to previous Server (If not first)
    Next: TWOL_ServerData;          // Pointer to next Server (If not last)

    function ValidSKU(SKU: String): Boolean;
  end;


  TConfigInfo = record
     SQLInfo: TSQLInfo;
     SQLDB: TSQL;

     PIDName: String;
     PIDFile: Text;
     IsForked: Boolean;
     ListenHost: String;
     ListenPort: integer;
     StartTime: longint;
     ServServ: Boolean;
     MyName: String;
     BotName: String;
     ShowOfficial: Boolean;
     CanChat: Boolean;
//     ChanMode: Integer;
     LockStatus: Boolean;
     SKUList: TStringList;
     ChanModeList: TStringList;
     PlayableChannels: Integer;
     MOTD: String;    
  end;

  rClanInfo = record
    ID: Integer;
    Name: String;
    Tag: String;
    Rank: Integer;
    Owner: String;
    Password: String;
    Time: LongInt;
    Website: String;
    Open: Boolean;
  end;

var

  Config: TConfigInfo;

  UserDataTotal: integer;
  FirstUserData: TWOL_UserData;
  LastUserData: TWOL_UserData;

  ServerDataTotal: integer;
  FirstServerData: TWOL_ServerData;
  LastServerData: TWOL_ServerData;

  procedure LL_InitDataFile();
  function LL_AddUserData(AddFirst: Boolean = True): TWOL_UserData;
  procedure LL_DeleteUserData(UserData: TWOL_UserData);
  function LL_FindUserData(UserName: String): TWOL_UserData;

  function LL_AddServerData(AddFirst: Boolean = True): TWOL_ServerData;
  procedure LL_DeleteServerData(ServerData: TWOL_ServerData);
  function LL_FindServerData(CName: String): TWOL_ServerData;

//  function GetHeaders(FileName: String): TStringList;
//  function SaveDataFile(): boolean;
//  function ParseDataFile(): boolean;
  procedure ParseDataFile();
  procedure GetGameServSettings();
  procedure GetServServSettings();
  procedure GetServersSettings();

  function LoadUserData(sName: String): TWOL_UserData;
  function SaveUserData(UserData: TWOL_UserData): Boolean;
  function GetUsersCount(): Integer;
  function ValidChanMode(Mode: String): Boolean;
  function ValidSKU(SKU: String; SKUList: TStringList = nil): Boolean;
  function GetClanInfo(nID: Integer): rClanInfo;
  function GetClanID(sName: String; UseName: Boolean = False): Integer;
  function SetClanInfo(var Info: rClanInfo): Boolean;
  function GetUserClan(sUsername: String): Integer;
  function GetSerialCount(sSerial: String): Integer;

implementation
uses Common, UserSocket;

function ValidChanMode(Mode: String): Boolean;
begin
  Result := ValidSKU(Mode,Config.ChanModeList);
end;

function TWOL_ServerData.ValidSKU(SKU: String): Boolean;
begin
  Result := datafile.ValidSKU(SKU,Self.SKUList);
end;

function ValidSKU(SKU: String; SKUList: TStringList = nil): Boolean;
var
  I: Integer;
begin
  Result := False;
  if SKUList = nil then SKUList := Config.SKUList;
  if SKUList = nil then exit;
  for I := 0 to SKUList.count-1 do begin
    if lowercase(SKU) = lowercase(SKUlist[I]) then begin
      Result := True;
      Break;
    end;
  end;
end;

procedure LL_InitUserDatas();
var
  ptr: TWOL_UserData;
begin
  if FirstUserData <> nil then begin
    ptr := FirstUserData;
    while ptr <> nil do begin
      if ptr.prev <> nil then begin
        LL_DeleteUserData(ptr.prev);
      end;
      if ptr.next <> nil then begin
        ptr := ptr.next
      end else begin
        LL_DeleteUserData(ptr);
        break;
      end;
    end;
  end;

  LastUserData := nil;
  FirstUserData := nil;
  UserDataTotal := 0;
end;

//----------------------------------------------------------------------------
// Add a new UserData object
// AddFirst is used to decide if it is added at the start or end
//----------------------------------------------------------------------------
function LL_AddUserData(AddFirst: Boolean = True): TWOL_UserData;
var
  NewUserData: TWOL_UserData;
begin
  NewUserData := TWOL_UserData.Create;

  if AddFirst then begin
    NewUserData.Next := FirstUserData;
    NewUserData.Prev := nil;
    if UserDataTotal = 0 then begin
       LastUserData := NewUserData;
    end else begin
       FirstUserData.Prev := NewUserData;
    end;
    FirstUserData := NewUserData;
  end else begin
    NewUserData.Prev := LastUserData;
    NewUserData.Next := nil;
      if UserDataTotal = 0 then begin
        FirstUserData := NewUserData;
      end else begin
        LastUserData.Next := NewUserData;
      end;
    LastUserData := NewUserData;
  end;

  NewUserData.name := '';
  NewUserData.password := '';
  NewUserData.serial := '';  
  NewUserData.isadmin := False;
  NewUserData.LastConnected := 0;
  NewUserData.Time := 0;
  NewUserData.CodePage := 0;
  NewUserData.Locale := 0;
  NewUserData.Clan := 0;
  NewUserData.InClan := False;
  NewUserData.NoSave := False;


  Inc(UserDataTotal);
  Result := NewUserData;
end;

//----------------------------------------------------------------------------
// Delete a UserData object
//----------------------------------------------------------------------------
procedure LL_DeleteUserData(UserData: TWOL_UserData);
begin
  if UserData = nil then exit;

  if not assigned(UserData.Prev) then begin
    // i.e., the UserData is first
    FirstUserData := UserData.Next;
  end;

  if not assigned(UserData.next) then begin
    // i.e., the UserData is last
    LastUserData := UserData.Prev;
  end;

  if UserData.Prev <> nil then begin
    UserData.Prev.Next := UserData.Next;
  end;

  if UserData.Next <> nil then begin
    UserData.Next.Prev := UserData.Prev;
  end;

  FreeAndNil(UserData);
  Dec(UserDataTotal);
end;

//----------------------------------------------------------------------------
// Find a UserData object
// Nickname is matched as lowercase
// First match is returned
//----------------------------------------------------------------------------
function LL_FindUserData(UserName: String): TWOL_UserData;
var
  ptr: TWOL_UserData;
begin
  Result := nil;
  ptr := FirstUserData;
  while ptr <> nil do begin
    if lowercase(ptr.Name) = lowercase(UserName) then begin
      Result := ptr;
      break;
    end;
    ptr := ptr.next;
  end;
end;

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
procedure LL_InitServerDatas();
var
  ptr: TWOL_ServerData;
begin
  if FirstServerData <> nil then begin
    ptr := FirstServerData;
    while ptr <> nil do begin
      if ptr.prev <> nil then begin
        LL_DeleteServerData(ptr.prev);
      end;
      if ptr.next <> nil then begin
        ptr := ptr.next
      end else begin
        LL_DeleteServerData(ptr);
        break;
      end;
    end;
  end;

  LastServerData := nil;
  FirstServerData := nil;
  ServerDataTotal := 0;
end;

//----------------------------------------------------------------------------
// Add a new ServerData object
// AddFirst is used to decide if it is added at the start or end
//----------------------------------------------------------------------------
function LL_AddServerData(AddFirst: Boolean = True): TWOL_ServerData;
var
  NewServerData: TWOL_ServerData;
begin
  NewServerData := TWOL_ServerData.Create;

  if AddFirst then begin
    NewServerData.Next := FirstServerData;
    NewServerData.Prev := nil;
    if ServerDataTotal = 0 then begin
       LastServerData := NewServerData;
    end else begin
       FirstServerData.Prev := NewServerData;
    end;
    FirstServerData := NewServerData;
  end else begin
    NewServerData.Prev := LastServerData;
    NewServerData.Next := nil;
      if ServerDataTotal = 0 then begin
        FirstServerData := NewServerData;
      end else begin
        LastServerData.Next := NewServerData;
      end;
    LastServerData := NewServerData;
  end;

  NewServerData.ConfigName := '';
  NewServerData.Name := '';  
  NewServerData.Port := 0;
  NewServerData.Description := '';

  NewServerData.SKUList := nil;


  Inc(ServerDataTotal);
  Result := NewServerData;
end;

//----------------------------------------------------------------------------
// Delete a ServerData object
//----------------------------------------------------------------------------
procedure LL_DeleteServerData(ServerData: TWOL_ServerData);
begin
  if ServerData = nil then exit;
  if ServerData.SKUList <> nil then begin
    ServerData.SKUList.Clear;
    ServerData.SKUList.Free;
    ServerData.SKUList := nil;
  end;

  if not assigned(ServerData.Prev) then begin
    // i.e., the ServerData is first
    FirstServerData := ServerData.Next;
  end;

  if not assigned(ServerData.next) then begin
    // i.e., the ServerData is last
    LastServerData := ServerData.Prev;
  end;

  if ServerData.Prev <> nil then begin
    ServerData.Prev.Next := ServerData.Next;
  end;

  if ServerData.Next <> nil then begin
    ServerData.Next.Prev := ServerData.Prev;
  end;

  ServerData.free;
  Dec(ServerDataTotal);
end;

//----------------------------------------------------------------------------
// Find a ServerData object
// Nickname is matched as lowercase
// First match is returned
//----------------------------------------------------------------------------
function LL_FindServerData(CName: String): TWOL_ServerData;
var
  ptr: TWOL_ServerData;
begin
  Result := nil;
  ptr := FirstServerData;
  while ptr <> nil do begin
    if lowercase(ptr.ConfigName) = lowercase(CName) then begin
      Result := ptr;
      break;
    end;
    ptr := ptr.next;
  end;
end;


procedure LL_InitDataFile();
begin
  LL_InitServerDatas();
  LL_InitUserDatas();
end;

procedure ParseDataFile();
var
  myINI: TIniFile;
  sFilename: String;
begin
  if (CLI.Exists('c') <> -1) then sFilename := CLI.Get(CLI.Exists('c')).sValue
  else if (CLI.Exists('conf') <> -1) then sFilename := CLI.Get(CLI.Exists('conf')).sValue
  else sFilename := './mysql.conf';

  DoWriteLN('[ParseDataFile] Reading from File: '+sFilename);

  myINI := TIniFile.Create(sFilename);
  Config.SQLInfo.sDBName := myINI.ReadString('mysql','Database','dwol');
  Config.SQLInfo.sDBUser := myINI.ReadString('mysql','Username','dwol');
  Config.SQLInfo.sDBPass := myINI.ReadString('mysql','Password','dwol');
  Config.SQLInfo.sDBHost := myINI.ReadString('mysql','Hostname','127.0.0.1');
  Config.SQLInfo.nDBPort := myINI.ReadInteger('mysql','Port', 3306);
  myINI.Destroy;
  Config.SQLDB.Setup(Config.SQLInfo);
  Config.SQLDB.Connect;
  Config.SQLDB.ResetQueryCount;
  if (Config.ServServ) then begin
    GetServServSettings();
    GetServersSettings();    
  end
  else begin
    GetGameServSettings();
  end;
end;

function GetUsersCount(): Integer;
begin
  Result := 0;
  Config.SQLDB.Query('Select Count(*) as C from Users');
  Config.SQLDB.GetNextRow;

  if Config.SQLDB.HasRow then begin
    Result := strtoint(Config.SQLDB.GetFieldValue('C'));
  end;
end;

function GetSerialCount(sSerial: String): Integer;
begin
  Result := 0;
  Config.SQLDB.Query('Select Count(*) as C from Users WHERE Serial='''+Config.SQLDB.Escape(sSerial)+'''');
  Config.SQLDB.GetNextRow;

  if Config.SQLDB.HasRow then begin
    Result := strtoint(Config.SQLDB.GetFieldValue('C'));
  end;
end;

function GetUserClan(sUsername: String): Integer;
begin
  Result := 0;
  Config.SQLDB.Query('Select Clan from Users where lower(Name)='''+Config.SQLDB.Escape(sUsername)+'''');
  Config.SQLDB.GetNextRow;

  if Config.SQLDB.HasRow then begin
    Result := strtoint(Config.SQLDB.GetFieldValue('Clan'));
  end;
end;

function GetClanID(sName: String; UseName: Boolean = False): Integer;
begin
  Result := -1;
  if UseName then Config.SQLDB.Query('Select * from Clans where lower(Name)='''+lowercase(Config.SQLDB.Escape(sName))+'''')
  else Config.SQLDB.Query('Select * from Clans where lower(Tag)='''+lowercase(Config.SQLDB.Escape(sName))+'''');
  Config.SQLDB.GetNextRow;
  if Config.SQLDB.HasRow then begin
    Result := StrToInt(Config.SQLDB.GetFieldValue('ID'));
  end;
end;

function GetClanInfo(nID: Integer): rClanInfo;
begin
  Result.ID := -1;
  Result.Name := '';
  Result.Tag := '';
  Result.Rank := -1;
  Result.Owner := '';
  Result.Password := '';
  Result.Time := 0;
  Result.Website := '';
  Result.Open := False;  

  Config.SQLDB.Query('Select * from Clans where ID='+inttostr(nID));
  Config.SQLDB.GetNextRow;
  if Config.SQLDB.HasRow then begin
    Result.ID := StrToInt(Config.SQLDB.GetFieldValue('ID'));
    Result.Name := Config.SQLDB.GetFieldValue('Name');
    Result.Tag := Config.SQLDB.GetFieldValue('Tag');
    Result.Rank := StrToInt(Config.SQLDB.GetFieldValue('Rank'));
    Result.Owner := Config.SQLDB.GetFieldValue('Owner');
    Result.Password := Config.SQLDB.GetFieldValue('Password');
    Result.Time := StrToInt(Config.SQLDB.GetFieldValue('Time'));
    Result.Website := Config.SQLDB.GetFieldValue('Website');
    Result.Open := IntToBool(StrToInt(Config.SQLDB.GetFieldValue('Open')));
  end;     
end;

function SetClanInfo(var Info: rClanInfo): Boolean;
begin
  Result := False;
  if (Info.ID < 1) and (Info.ID <> -2) then exit; // -2 = new clan
  Config.SQLDB.Query('Select * from Clans where ID='+inttostr(Info.ID));
  Config.SQLDB.GetNextRow;
  if Config.SQLDB.HasRow then begin
    // Old Clan
    Config.SQLDB.Query('UPDATE Clans SET `Name`='''+Config.SQLDB.Escape(Info.Name)+''',`Tag`='''+Config.SQLDB.Escape(Info.Tag)+''',`Owner`='''+Config.SQLDB.Escape(Info.Owner)+''',`Password`='''+Config.SQLDB.Escape(Info.Password)+''',`Website`='''+Config.SQLDB.Escape(Info.Website)+''',`Rank`='+inttostr(Info.Rank)+',`Time`='+inttostr(Info.Time)+',`Open`='+IntToStr(BoolToInt(Info.Open))+' WHERE ID='+IntToStr(Info.ID));
  end
  else begin
    // New Clan
    Config.SQLDB.Query('INSERT into Clans (Name,Tag,Owner,Password,Website,Time,Rank,Open) VALUES ('''+Config.SQLDB.Escape(Info.Name)+''','''+Config.SQLDB.Escape(Info.Tag)+''','''+Config.SQLDB.Escape(Info.Owner)+''','''+Config.SQLDB.Escape(Info.Password)+''','''+Config.SQLDB.Escape(Info.Website)+''','+IntToStr(Info.Time)+','+IntToStr(Info.Rank)+','+IntToStr(BoolToInt(Info.Open))+')');
    Info.ID := Config.SQLDB.GetInsertID;
  end;
  DoWriteLN('Query: '+Config.SQLDB.LastQuery);
  DoWriteLN('Error: '+Config.SQLDB.GetError);
  Result := True;
end;

function SaveUserData(UserData: TWOL_UserData): Boolean;
begin
  Result := False;
  if (UserData = nil) or (UserData.NoSave) then exit;
  if UserData.ID > 0 then Config.SQLDB.Query('Select * from Users where ID='''+Inttostr(UserData.ID)+'''')
  else Config.SQLDB.Query('Select * from Users where lower(Name)='''+UserData.Name+'''');
  Config.SQLDB.GetNextRow;
  if Config.SQLDB.HasRow then begin
    // Old User
    Config.SQLDB.Query('UPDATE Users SET `Password`='''+Config.SQLDB.Escape(UserData.Password)+''',`Serial`='''+Config.SQLDB.Escape(UserData.Serial)+''',`IsAdmin`='+IntToStr(BoolToInt(UserData.IsAdmin))+',`Clan`='+IntToStr(UserData.Clan)+',`LastConnected`='+IntToStr(UserData.LastConnected)+',`CreationTime`='+IntToStr(UserData.Time)+',`Locale`='+IntToStr(UserData.Locale)+',`CodePage`='+IntToStr(UserData.CodePage)+' WHERE `ID`='''+Inttostr(UserData.ID)+'''');
  end
  else begin
    // New User
    Config.SQLDB.Query('INSERT into Users (`Name`,`Password`,`Serial`,`IsAdmin`,`Clan`,`LastConnected`,`CreationTime`,`Locale`,`CodePage`) VALUES ('''+Config.SQLDB.Escape(UserData.Name)+''','''+Config.SQLDB.Escape(UserData.Password)+''','''+Config.SQLDB.Escape(UserData.Serial)+''','+IntToStr(BoolToInt(UserData.IsAdmin))+','+IntToStr(UserData.Clan)+','+IntToStr(UserData.LastConnected)+','+IntToStr(UserData.Time)+','+IntToStr(UserData.Locale)+','+IntToStr(UserData.CodePage)+')');
    UserData.ID := Config.SQLDB.GetInsertID;
  end;
  DoWriteLN('Query: '+Config.SQLDB.LastQuery);
  DoWriteLN('Error: '+Config.SQLDB.GetError);
  Result := True;
end;

function LoadUserData(sName: String): TWOL_UserData;
begin
  Result := ll_finduserdata(sName);
  if Result = nil then Result := ll_adduserdata();

  Config.SQLDB.Query('Select * from Users where lower(Name)='''+Lowercase(sName)+'''');
  Config.SQLDB.GetNextRow;

  if Config.SQLDB.HasRow then begin
    Result.ID := StrToInt(Config.SQLDB.GetFieldValue('ID'));
    Result.Name := sName;
    Result.Password := Config.SQLDB.GetFieldValue('Password');
    Result.Serial := Config.SQLDB.GetFieldValue('Serial');
    Result.isadmin := inttobool(strtoint(Config.SQLDB.GetFieldValue('IsAdmin')));
    Result.Time := strtoint(Config.SQLDB.GetFieldValue('CreationTime'));
    Result.LastConnected := strtoint(Config.SQLDB.GetFieldValue('LastConnected'));
    Result.CodePage := strtoint(Config.SQLDB.GetFieldValue('CodePage'));
    Result.Locale := strtoint(Config.SQLDB.GetFieldValue('Locale'));
    Result.Clan := strtoint(Config.SQLDB.GetFieldValue('Clan'));
    if GetClanInfo(Result.Clan).ID < 1 then Result.Clan := 0;
    Result.InClan := (Result.Clan <> 0);
    Result.IsNew := False;    
  end
  else begin
    Result.ID := -1;     
    Result.Name := sName;
    Result.Password := '';
    Result.Serial := '';
    Result.isadmin := False;
    Result.Time := unixtime();
    Result.LastConnected := unixtime();
    Result.CodePage := 1250;
    Result.Locale := 0;
    Result.Clan := 0;
    Result.InClan := False;    
    Result.IsNew := True;
  end;
end;


procedure GetGameServSettings();
var
  sId,sTemp: String;
begin
  if (CLI.Exists('i') <> -1) then sId := CLI.Get(CLI.Exists('i')).sValue
  else if (CLI.Exists('id') <> -1) then sId := CLI.Get(CLI.Exists('id')).sValue
  else sId := '';

  if sID = '' then Config.SQLDB.Query('Select * from Servers Order by ID DESC Limit 1')
  else Config.SQLDB.Query('Select * from Servers WHERE ID='+Config.SQLDB.Escape(sID));
  Config.SQLDB.GetNextRow;

  if Config.SQLDB.HasRow then begin
    Config.MyName := Config.SQLDB.GetFieldValue('HostName');  
    Config.ListenPort := strtoint(Config.SQLDB.GetFieldValue('ListenPort'));
    Config.PlayableChannels := strtoint(Config.SQLDB.GetFieldValue('GameChannels'));
    Config.MOTD := Config.SQLDB.GetFieldValue('motd');    
    sTemp := Config.SQLDB.GetFieldValue('SKUList');
    Split(' ',sTemp,Config.SKUList);
    sTemp := Config.SQLDB.GetFieldValue('ChanMode');
    Split(' ',sTemp, Config.ChanModeList);
    Config.BotName := Config.SQLDB.GetFieldValue('BotName');
    Config.CanChat := inttobool(strtoint(Config.SQLDB.GetFieldValue('CanChat')));
    Config.LockStatus := inttobool(strtoint(Config.SQLDB.GetFieldValue('GamesLocked')));
  end
  else begin
    DoWriteLN('Unable to load data from mySQL Table! Check your settings.');
    DoWriteLN('Query: '+Config.SQLDB.LastQuery);    
    DoWriteLN('Error: '+Config.SQLDB.GetError);
    messageloop;
  end;
end;

procedure GetServServSettings();
begin
  Config.ListenPort := 4005;
  Config.ShowOfficial := False;

  GetServersSettings();
end;

procedure GetServersSettings();
var
  sName,sTemp: String;
  Sptr: TWOL_ServerData;
begin
  Config.SQLDB.Query('Select * from Servers');
  Config.SQLDB.GetNextRow;

  while Config.SQLDB.HasRow do begin
    sName := Config.SQLDB.GetFieldValue('ID');
    Sptr := ll_findserverdata(sName);
    if Sptr = nil then Sptr := ll_addserverdata();
    Sptr.ConfigName := sName;
    Sptr.Name := Config.SQLDB.GetFieldValue('FriendlyName');
    Sptr.Host := Config.SQLDB.GetFieldValue('HostName');
    Sptr.Description := Config.SQLDB.GetFieldValue('Description');
    Sptr.Port := strtoint(Config.SQLDB.GetFieldValue('ListenPort'));
    sTemp := Config.SQLDB.GetFieldValue('SKUList');
    Split(' ',sTemp, Sptr.SKUList);
    DoWriteLN('-> Found Server: '+Sptr.Name);
    Config.SQLDB.GetNextRow;
  end;
end;

{---- Adjust global SVN revision ----}
initialization
  Config.SKUList := nil;
  Config.ChanModeList := nil;
  Config.SQLDB := TSQL.Create();
  SVNRevision('$Id$');
finalization
  Config.SQLDB.Destroy;

  if Config.ChanModeList <> nil then begin
    Config.ChanModeList.Clear;
    Config.ChanModeList.Free;
    Config.ChanModeList := nil;
  end;
  if Config.SKUList <> nil then begin
    Config.SKUList.Clear;
    Config.SKUList.Free;
    Config.SKUList := nil;
  end;
end.
