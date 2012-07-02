{*
 * forking - Allow forking under *nix
 * Copyright (C) 2005 Shane "Dataforce" Mc Cormack
 * For conditions of distribution and use, see copyright notice in license.txt
 * 
 * Linux Based on unitfork.pas from bewareserv.
 * Unitfork: http://cvs.sourceforge.net/viewcvs.py/bewareserv/bewareserv/unitfork.pas?rev=1.7&view=markup
 * Permission granted to use under ZLIB License.
 * 
 * <Dataforce> Beware: I dont suppose it would be possible to use unitfork as zlib rather than GPL is it ?
 * <beware> yes
 * <beware> it should be, even
 * <beware> hm
 * <beware> hang on
 * <beware> yes that shouls be core
 *
 * SVN: $Id$
 *}

unit forking;

interface

uses
  {$IFNDEF WIN32}{$ifdef VER1_0}linux,{$else}baseunix,unix,unixutil,{$endif}{$ENDIF} sysutils, datafile, common, svn;

function DoFork(): boolean;
function WritePID(Name: String): Boolean;
function CheckPID(Name: string): boolean;
function DeletePID(): Boolean;


implementation

{$IFDEF WIN32}
// Windows stuff

function DoFork(): boolean;
begin
  Result := False;
end;

function GetPID(): String;
begin
  Result := 'Unknown'; // Might make this do somamt someday
end;

function WritePID(Name: String): Boolean;
begin
  try
//    Assignfile(Config.PIDFile,Name);
//    rewrite(Config.PIDFile);
//    WriteLN(Config.PIDFile,GetPID);
//    Flush(Config.PIDFile);
//    Close(Config.PIDFile);
    Result := True;
  except
    Result := False;
  end;
end;

function CheckPID(Name: string): boolean;
begin
  Result := True;
end;

function DeletePID(): Boolean;
begin
  Result := True;
end;

{$ELSE}
// Linux

function DoFork(): boolean;
var
  ForkResult: integer;
begin
  ForkResult := fpfork;
  Result := True;
  if ForkResult <> 0 then halt;
end;

function WritePID(Name: String): Boolean;
begin
  try
    WriteLN(Config.PIDFile,FPGetPID);
    Flush(Config.PIDFile);
    Result := True;
  except
    Result := False;
  end;
  if Name <> '' then exit; // This utterly pointless line prevents compiler warnings
end;

function DeletePID(): Boolean;
begin
  result := false;
  if Config.PIDName = '' then exit;
  try
    {$i-}
    Closefile(Config.PIDFile);
    Erase(Config.PIDFile);
    {$i+}
    DoWriteLN('[DeletePID] Delete successful.');
    result := true;
  except
    DoWriteLN('[DeletePID] Delete failed.');
  end;
  Config.PIDName := '';
end;

function CheckPID(Name: string): boolean;
var
  Handle: THandle;
begin
  Result := False;
  Config.PIDName := '';
  Assignfile(Config.PIDFile,Name);

  filemode := 2;

  {$i-}Reset(Config.PIDFile);{$i+}
  if ioresult <> 0 then {$i-}rewrite(Config.PIDFile);{$i+}
  if ioresult <> 0 then exit;

  Handle := GetFS(Config.PIDFile);

  if fpflock(Handle,LOCK_EX or LOCK_NB) <> 0 then begin
    DoWriteLN('[CheckPID] Unable to get a lock on PIDFile');
    close(Config.PIDFile);
    exit;
  end;
  rewrite(Config.PIDFile);

  if fpflock(Handle,LOCK_EX or LOCK_NB) <> 0 then begin
    DoWriteLN('[CheckPID] Lock Failed');
    close(Config.PIDFile);
    exit;
  end;
  Config.PIDName := Name;
  Result := True;
end;

{$ENDIF}

{---- Adjust global SVN revision ----}
initialization
  SVNRevision('$Id$');
end.
