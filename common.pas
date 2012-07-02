{*
 * Common - Common functions used by multiple files.
 * Copyright (C) 2005 Chris "MD87" Smith, Shane "Dataforce" Mc Cormack
 * For conditions of distribution and use, see copyright notice in license.txt
 *
 * SVN: $Id: common.pas 41 2006-03-03 21:52:10Z Dataforce $
 *}
unit common;

{$I defines.inc}

interface

uses
  {$ifdef win32}WSocket, WCore,{$else}lsocket, lcore,{$endif} DebugList,
  SysUtils, Classes, fastmd5, nixtime, ControlSocket, UserSocket, datafile, svn, CLIParser;

Const
  {$I consts.inc}
  DEBUG_IN = 1;
  DEBUG_OUT = 2;
  DEBUG_PROC = 4;

type
{$IFDEF FPC}
  TSystemTime = record
     wYear: Word;
     wMonth: Word;
     wDayOfWeek: Word;
     wDay: Word;
     wHour: Word;
     wMinute: Word;
     wSecond: Word;
     wMilliseconds: Word;
  end;
  {$ENDIF}

   TDebug = record
     DataIn: TDebugList;
     DataOut: TDebugList;
     ProcCall: TDebugList;
     All: TDebugList;
   end;

  {$ifdef win32}
    TSocket = TWSocket;
  {$else}
    TSocket = Tlsocket;
  {$endif}

var
    DebugInfo: TDebug;
    EndedLoop: Boolean;
    CLI: TCLIParser;

  procedure Split(Deliminator: Char; Target: String; var Res: TStringList);
  function Join(Glue: String; Target: TStringList): String; overload;
  function Join(Glue: String; Target: TStringList; Start,Finish: Integer): String; overload;
  function JoinSplit(Glue: String; Deliminator: Char; Target: String): String; overload;
  function JoinSplit(Glue: String; Deliminator: Char; Target: String; Start,Finish: Integer): String; overload;
  function gettok (buff: String; pos: Integer; ascii: Integer): String;
  function GetParams(Line: String): TStringList;
  function gettoks (buff: String; startpos: Integer; endpos: Integer; ascii: Integer): String;
  function numtok (buff: String; ascii: Integer): Integer;

  function SendLine(sendsock:TSocket;Line:String): boolean;

  procedure DoWriteLn(S: String);
  procedure DoWrite(S: String);
  Function isfloat(S: String; AllowNegative: Boolean = False): Boolean;
  Function isint(S: String; AllowNegative: Boolean = False): Boolean;

  function BoolToStr(aBoolean: Boolean; bBoolean: Boolean = false): String;
  function IntToBool(nInteger: Integer): Boolean;
  function BoolToInt(aBoolean: Boolean): Byte;

  function ValidIP(IP: String): Boolean;

  procedure AddDebug(List: Integer; Data: String);
  procedure InitDebug();

  procedure EndOfMessageLoop();
  function DoDebugTrace(E: Exception; Place: String; ReturnOnly: Boolean = false; AdminOnly: Boolean = false): TStringList;

  function FormatSize(TheSize: Int64): String;
  function ReadToList(FileName: String): TStringList;

  function MakeAPGAR(const Password: String): String; overload;
  function MakeAPGAR(const Password: PChar): String; overload;

implementation
uses forking;

function ReadToList(FileName: String): TStringList;
var
  tFile: TextFile;
  S: String;
begin
  Result := TStringList.Create();
  if FileExists(FileName) then begin
    AssignFile(tFile, FileName);
    Reset(tFile);
    while not Eof(tFile) do begin
      ReadLn(tFile, S);
      Result.Add(S);
    end;
    CloseFile(tFile);
  end;
end;

function FormatSize(TheSize: Int64): String;
const
  MB_SIZE: Integer = 1048576;
  KB_SIZE: Integer = 1024;
var
  MB: integer;
  KB: integer;
  B: integer;
begin
  MB := TheSize div MB_SIZE;
  TheSize := TheSize - (MB * MB_SIZE);

  KB := TheSize div KB_SIZE;
  TheSize := TheSize - (KB * KB_SIZE);

  B := TheSize;

  Result := '';
  if MB > 0 then Result := Result+inttostr(MB)+' MByte';
  if MB > 1 then Result := Result+'s ' else Result := Result+' ';

  if KB > 0 then Result := Result+inttostr(KB)+' KByte';
  if KB > 1 then Result := Result+'s ' else Result := Result+' ';

  if B > 0 then Result := Result+inttostr(B)+' Byte';
  if B > 1 then Result := Result+'s ' else Result := Result+' ';
  Result := trim(Result);
end;

procedure InitDebug();
begin
  DebugInfo.DataIn := TDebugList.create(15);
  DebugInfo.DataOut := TDebugList.create(15);
  DebugInfo.ProcCall := TDebugList.create(30);
  DebugInfo.All := TDebugList.create(60);
end;

procedure EndOfMessageLoop();
begin
  if EndedLoop then exit;
  DoDebugTrace(nil, 'EndOfMessageLoop'); // This is so that its easy to see the path that called EndOfMessageLoop
  DeletePID();
  LL_InitSockets();
  DoWriteln('[dWOL] Bye Bye.');
  EndedLoop := true;
  halt;
end;

function DoDebugTrace(E: Exception; Place: String; ReturnOnly: Boolean = false; AdminOnly: Boolean = false): TStringList;
var
  ErrorInfoData: TStringList;
  Time: String;
  myFile: TextFile;
  I: Integer;
begin
  ErrorInfoData := TStringList.Create();
  Time := FormatDateTime('dd mmm yyyy/hh:nn:ss',UnixToDateTime(UnixTime));
  ErrorInfoData.Add('--------------------------');
  try
    ErrorInfoData.Add('Error Detected at '+Time+'. ['+Place+'] Traces To Follow.');
    if E <> nil then begin
      ErrorInfoData.Add('E.Message: '+E.Message);
      ErrorInfoData.Add('E.Classname: '+E.ClassName);
    end;
    ErrorInfoData.Add('InComming Data:');
    For I := 0 to DebugInfo.DataIn.MaxItem do begin
      ErrorInfoData.Add('     ['+inttostr(I)+']> '+DebugInfo.DataIn.GetItem(I));
    end;
    ErrorInfoData.Add('Outgoing Data:');
    For I := 0 to DebugInfo.DataOut.MaxItem do begin
      ErrorInfoData.Add('     ['+inttostr(I)+']> '+DebugInfo.DataOut.GetItem(I));
    end;
    ErrorInfoData.Add('Procedure Call Stack:');
    For I := 0 to DebugInfo.ProcCall.MaxItem do begin
      ErrorInfoData.Add('     ['+inttostr(I)+']> '+DebugInfo.ProcCall.GetItem(I));
    end;
    ErrorInfoData.Add('Full Trace:');
    For I := 0 to DebugInfo.All.MaxItem do begin
      ErrorInfoData.Add('     ['+inttostr(I)+']> '+DebugInfo.All.GetItem(I));
    end;
  except
    on Ex: Exception do begin
      ErrorInfoData.Add('----{[ Error Getting Error Data ]}----');
      ErrorInfoData.Add('Ex.Message: '+Ex.Message);
      ErrorInfoData.Add('Ex.Classname: '+Ex.ClassName);
      ErrorInfoData.Add('----{[ Error Getting Error Data ]}----');      
    end;
  end;
  try
    if Not ReturnOnly then begin
      AssignFile(myFile, './Error.log');
      Append(myFile);
      WriteLn(myFile,' ');
      WriteLn(myFile,' ');
      WriteLn(myFile,' ');
      For I := 0 to ErrorInfoData.Count-1 do begin
        DoWriteLn(ErrorInfoData.Strings[I]);
        WriteLn(myFile,ErrorInfoData.Strings[I]);
      end;
      CloseFile(myFile);
    end;
  except
    on Ex: Exception do begin
      ErrorInfoData.Add('----{[ Error Writing Error Data to File ]}----');
      ErrorInfoData.Add('Ex.Message: '+Ex.Message);
      ErrorInfoData.Add('Ex.Classname: '+Ex.ClassName);
      ErrorInfoData.Add('----{[ Error Writing Error Data to File ]}----');
    end;
  end;
  Result := ErrorInfoData;

  ErrorInfoData.Clear;
  ErrorInfoData.Destroy;
end;

procedure AddDebug(List: Integer; Data: String);
var
  Time: String;
  LogType: String;
begin
  Time := FormatDateTime('dd mmm yyyy/hh:nn:ss',UnixToDateTime(UnixTime));
  if (DEBUG_IN and List) <> 0 then begin
    DebugInfo.DataIn.AddItem('['+Time+'] '+Data);
    LogType := 'In';
  end;
  if (DEBUG_OUT and List) <> 0 then begin
    DebugInfo.DataOut.AddItem('['+Time+'] '+Data);
    if LogType = '' then Logtype := 'Out' else Logtype := LogType+'/Out';
  end;
  if (DEBUG_PROC and List) <> 0 then begin
    DebugInfo.ProcCall.AddItem('['+Time+'] '+Data);
    if LogType = '' then Logtype := 'Proc' else Logtype := LogType+'/Proc';
  end;
  DebugInfo.All.AddItem('['+Time+'] ('+LogType+') '+Data);
end;


function ValidIP(IP: String): Boolean;
var
  bits: TStringList;
  I: integer;
begin
  AddDebug(DEBUG_PROC,'ValidIP');
  bits := nil;
  split('.',IP,bits);
  result := false;
  if bits.Count <> 4 then exit;
  for I := 0 to 3 do begin
    if not isint(bits[I]) then exit;
    if I > 255 then exit;
  end;
  result := true;
  bits.clear;
  bits.Free;
end;

function IntToBool(nInteger: Integer): Boolean;
begin
  if nInteger = 1 then Result := True
  else Result := False;
end;

function BoolToInt(aBoolean: Boolean): Byte;
begin
  if aBoolean then Result := 1
  else result := 0;
end;


function BoolToStr(aBoolean: Boolean; bBoolean: Boolean = false): String;
begin
  if aBoolean then Result := 'True'
  else result := 'False';
end;

Function isint(S: String; AllowNegative: Boolean = False): Boolean;
var
  i: integer;
begin
  Result := true;
  if S <> '' then begin
    for i := 1 to length(S) do begin
      if (ord(S[i]) < 48) or (ord(S[i]) > 57) then begin
        if (ord(S[i]) = 45) and (I = 1) and (AllowNegative) then continue;
        Result := false;
        break;
      end;
    end;
  end
  else Result := false;
end;

Function isfloat(S: String; AllowNegative: Boolean = False): Boolean;
var
  i: integer;
  HasPoint: boolean;
begin
  Result := true;
  HasPoint := false;
  if S <> '' then begin
    for i := 1 to length(S) do begin
      if (ord(S[i]) < 48) or (ord(S[i]) > 57) then begin
        if ord(S[i]) = 46 then begin
          if Not HasPoint then begin
            HasPoint := true;
            continue;
          end;
        end;
        if (ord(S[i]) = 45) and (I = 1) and (AllowNegative) then continue;
        Result := false;
        break;
      end;
    end;
  end
  else Result := false;
end;

procedure DoWriteLn(S: String);
begin
  if isconsole and (not Config.IsForked) then WriteLN(S);
end;

procedure DoWrite(S: String);
begin
  if isconsole then Write(S);
end;

function SendLine(sendsock:TSocket;Line:String): boolean;
var
  sAddr: String;
begin
  AddDebug(DEBUG_PROC,'SendLine');
  if sendsock = nil then exit;
  if sendsock.state = wsConnected then begin
    sAddr := sendsock.getpeeraddr();
    DoWriteLN('['+sAddr+' OUT]:> '+line);
    sendsock.SendStr(Line+#13#10);
    Result := true;
  end
  else begin
    Result := False;
  end;
end;

function GetParams(Line: String): TStringList;
var
  I: Integer;
  T: String;
begin
  Result := TStringList.create();
  for I := 1 to length(Line) do begin
    if Line[I] <> ' ' then begin
      T := T+Line[I];
    end
    else begin
      Result.Add(T);
      T := '';
    end;
    if Line[I]+Line[I+1] = ' :' then begin
      T := copy(Line,I+2,length(Line)-I-1);
      Result.Add(T);
      exit;
    end;
  end;
  if T <> '' then begin
    Result.Add(T);
  end;
end;

function gettoks (buff: String; startpos: Integer; endpos: Integer; ascii: Integer): String;
var
  i: integer;
  Buffer: String;
  d: boolean;
begin
  d := false;
  if endpos = -1 then begin
    endpos := numtok(buff,ascii)-1;
  end;
  Buffer := '';
  for i := startpos to endpos do begin
    if d = false then begin
      Buffer := gettok(buff,i,ascii)
      end
    else begin
      Buffer := Buffer + chr(ascii) + gettok(buff,i,ascii)
    end;
    d := true;
  end;
  Result := Buffer;
end;

function numtok (buff: String; ascii: Integer): Integer;
var
  i: integer;
  r: integer;
begin
  r := 1;
  if buff <> '' then begin
    for i := 1 to length(Buff) do begin
      if buff[i] = chr(ascii) then begin
        r := r + 1;
      end;
    end;
  end;
  Result := r;
end;

function gettok (buff: String; pos: Integer; ascii: Integer): String;
var
  i: integer;
  Buffer: String;
begin
  Buffer := '';
  If pos > numtok(buff,ascii)-1 then begin
    Result := '';
    exit;
  end;
  If numtok(buff,ascii) = 1 then begin
    If pos > numtok(buff,ascii) then begin
      Result := '';
    end
    else begin
      Result := buff;
    end;
  end
  else begin
    for i := 1 to length(Buff)+1 do begin
    if buff[i] = chr(ascii) then begin
      if pos = 0 then begin
        Result := Buffer;
        Break;
      end;
      pos := pos - 1;
      Buffer := '';
    end else begin
      Buffer := Buffer + copy(Buff,i,1);
    end;
  end;
  Result := Buffer;
  end;
end;


procedure Split(Deliminator: Char; Target: String; var Res: TStringList);
var
 i: Integer;
 t: String;
begin
 if Res <> nil then begin
  Res.free;
  Res := nil;
 end;
 Res := TStringList.Create;
 t := '';
 for i := 1 to Length(Target) do begin
  if Copy(Target,i,1) = Deliminator then begin
   Res.Add(t);
   t := '';
  end else
   t := t + Copy(Target,i,1);
 end;
 if t <> '' then Res.Add(t);
end;

function Join(Glue: String; Target: TStringList): String;
var
 i: Integer;
begin
 Result := '';

 if Target.Count = 0 then Exit;

 for i := 0 to Target.Count-1 do begin
  if Result <> '' then Result := Result + Glue;
  Result := Result + Target[i];
 end;
  
end;

function Join(Glue: String; Target: TStringList; Start,Finish: Integer): String;
var
 i: Integer;
begin
 Result := '';

 if Target.Count = 0 then Exit;

 if Start > Target.Count - 1 then Start := Target.Count - 1;
 if Finish > Target.Count - 1 then Finish := Target.Count - 1;
 if Finish = -1 then Finish := Target.Count - 1; 
 if Start > Finish then begin
  i := Finish;
  Finish := Start;
  Start := i;
 end;

 for i := Start to Finish do begin
  if Result <> '' then Result := Result + Glue;
  Result := Result + Target[i];
 end;
  
end;

function JoinSplit(Glue: String; Deliminator: Char; Target: String): String; overload;
var
  T: TStringList;
begin
  T := nil;
  Split(Deliminator, Target, T);
  Result := Join(Glue,T);
  T.Free;
end;

function JoinSplit(Glue: String; Deliminator: Char; Target: String; Start,Finish: Integer): String; overload;
var
  T: TStringList;
begin
  T := nil;
  Split(Deliminator, Target, T);
  Result := Join(Glue,T,Start,Finish);
  T.Free;
end;

function MakeAPGAR(const Password: String): String;
begin
  Result := MakeAPGAR(PChar(Password));
end;

function MakeAPGAR(const Password: PChar): String;
const
  APGARCharacters = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789./';
var
  cResult: Array of Char;
  I,C,L: integer;
begin
  L := Length(Password);
  Setlength(cResult,L+1);
  for I := 0 to L-1 do begin
    C := Ord(Password[i]);
    if ((C mod 2) <> 0) then C := ((C shl (C and 1) and Ord(Password[L-I])))
    else C := ((C xor Ord(Password[L-I])));
    C := (C and $3F)+1;
    cResult[I] := APGARCharacters[C];
  end;
  cResult[L] := #0;
  Result := PChar(cResult);
end;

{---- Adjust global SVN revision ----}
initialization
  SVNRevision('$Id: common.pas 41 2006-03-03 21:52:10Z Dataforce $');
end.
