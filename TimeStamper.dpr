{*
 * timestamper - Generates a file containing a timestamp.
 * Copyright (C) 2005 Shane "Dataforce" Mc Cormack
 * For conditions of distribution and use, see copyright notice in license.txt
 *
 *
 * Windows UTCNow derived from Date_utc,Time_utc and now_utc from btime.pas from Bewareserv
 * Linux UTCNow derived from unixtimeint from btime.pas from Bewareserv
 * FPC Compatible TSystemTime taken from Bewareserv
 * Bewareserv Copyright (C) 2005 Bas Steendijk and Peter Green
 * For conditions of distribution and use, see copyright notice in license.txt
 *
 *
 * SVN: $Id:$
 *}
program Timestamper;

uses {$IFDEF UNIX}{$IFDEF VER1_0}linux, {$ELSE}baseunix, unix,{$ENDIF}{$ELSE}Windows, {$ENDIF}
      sysutils;

//-----------------------------------------------------------------------------
//  UTCNow
//  Equivilent of 'now' but taking into TimeZone into consideration and
//  retuning the UTC time, not the local time.
//-----------------------------------------------------------------------------
{$IFDEF UNIX}
  // Based on btime "unixtimeint"
  function UTCNow(): TDateTime;
  var
    tv: ttimeval;
    res: longint;
  begin
    fpgettimeofday(@tv,nil);
    res := tv.tv_sec;
    Result := (res / 86400) + 25569.0;  // Same line as used in Unixtodatetime
  end;
{$ELSE}
  {$IFDEF FPC}
  type
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
  
  // Based on btime "Date_utc", "Time_utc" and "now_utc"
  function UTCNow(): TDateTime;
  var
    SystemTime: TSystemTime;
  begin
    {$ifdef fpc}
      GetsystemTime(@SystemTime);
    {$else}
      GetsystemTime(SystemTime);
    {$endif}
    with SystemTime do
      Result := round(EncodeDate(wYear, wMonth, wDay)) + EncodeTime(wHour, wMinute, wSecond, wMilliSeconds);
  end;
{$ENDIF}

function Unixtime(): Longint;
Const
  UnixStartDate: TDateTime = 25569.0;
begin
  Result := Round((UTCNow() - UnixStartDate) * 86400);
end;


var
  F: Text;
  S: String;
  I: Integer;
begin
  S := 'time.inc';
  if ParamCount > 0 then begin
    for i := 1 to ParamCount do S := S+' '+ParamStr(I);
  end;
  S := Trim(S);
  AssignFile(F,S);
  Rewrite(F);
  WriteLN(F,'BUILDTIME = '+inttostr(unixtime())+';');
  CloseFile(F);
end.