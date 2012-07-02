{*
 * nixtime - Time functions for working with *nix time stamps
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
 * SVN: $Id:$
 *}
unit nixtime;

interface

uses
   {$IFDEF UNIX}{$IFDEF VER1_0}linux, {$ELSE}baseunix, unix,{$ENDIF}{$ELSE}Windows, {$ENDIF}
   SysUtils, svn;

  function DateTimeToUnix(ConvDate: TDateTime): Longint;
  function UnixToDateTime(UnixTime: Longint): TDateTime;
  function UnixTime(): LongInt;
  function UTCNow(): TDateTime;

implementation

//-----------------------------------------------------------------------------
//  DateTimeToUnix
//  Returns a Unix Timestamp as a LongInt of the time given as a TDateTime
//-----------------------------------------------------------------------------
function DateTimeToUnix(ConvDate: TDateTime): Longint;
Const
  // Sets UnixStartDate to TDateTime of 01/01/1970
  UnixStartDate: TDateTime = 25569.0;
begin
  //example: DateTimeToUnix(now);
  Result := Round((ConvDate - UnixStartDate) * 86400);
end;

//-----------------------------------------------------------------------------
//  UnixToDateTime
//  Returns a Timestamp as a TDateTime of the time given as a LongInt
//-----------------------------------------------------------------------------
function UnixToDateTime(UnixTime: Longint): TDateTime;
var
  Bias: TDateTime;
begin
  Bias := Now - UTCNow;
  Result := (UnixTime / 86400) + 25569.0;
  Result := Result + Bias;
end;


//-----------------------------------------------------------------------------
//  UnixTime
//  Returns the current time as a unixtime longint
//-----------------------------------------------------------------------------
function UnixTime(): LongInt;
begin
  Result:= DateTimeToUnix(UTCNow);
end;

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


{---- Adjust global SVN revision ----}
initialization
  SVNRevision('$Id:$');
end.