{*
 * dWOL - A Westwood Online Implementation
 * Copyright (C) 2005 Shane "Dataforce" Mc Cormack
 *
 * This software is provided 'as-is', without any express or implied warranty.
 * In no event will the authors be held liable for any damages arising from the
 * use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose, including
 * commercial applications, and to alter it and redistribute it freely, subject to
 * the following restrictions:
 *
 *     1. The origin of this software must not be misrepresented; you must not
 *        claim that you wrote the original software. If you use this software in a
 *        product, an acknowledgment in the product documentation would be
 *        appreciated but is not required.
 *
 *     2. Altered source versions must be plainly marked as such, and must not be
 *        misrepresented as being the original software.
 *
 *     3. This notice may not be removed or altered from any source distribution.
 *
 *
 * SVN: $Id$
 *}
program dWOL;

{$I defines.inc}
{$IFDEF WIN32}
  // Due to random dissapearings of the icon information, now stored separately.
  {$I win32.inc}
{$ENDIF}

uses
  svn,
  common,
  sysutils,
  nixtime,
  UserSocket,
  wol,
  datafile,
  ControlSocket,
  DebugList,
  Channels,
  CLIParser in 'CLIParser.pas',
  GameInfo in 'GameInfo.pas';

var
  I: Integer;
  ServServer: Boolean;
begin
  CLI := TCLIParser.Create();
  
  ServServer := false;
  SVNRevision('$Id$');
  DoWriteLN('------------------');
  DoWriteLN('   dWOL Version: '+VERSION_STRING);
  DoWriteLN('    Compiled at: '+FormatDateTime('dd mmm yyyy/hh:nn:ss',UnixToDateTime(BUILDTIME)));
  DoWriteLN('   SVN Revision: '+inttostr(SVN_REVISION));
  DoWriteLN('------------------');
  if (CLI.Exists('h') <> -1) Or (CLI.Exists('help') <> -1) then begin
    DoWriteLN('------------------');
    DoWriteLN('       Info       ');
    DoWriteLN('------------------');
    if SVN_DETAILS.Count > 0 then begin
      DoWriteLN('SVN Files:');
      for I := 0 to SVN_DETAILS.Count-1 do DoWriteLn('      ['+inttostr(I+1)+'] '+SVN_DETAILS.Strings[I]);
    end
    else begin
      DoWriteLN('Unable to obtain SVN Information.');
    end;
    DoWriteLN('------------------');
    DoWriteLN('dWOL Information: http://home.dataforce.org.uk/wiki/index.php?dWOL');
    DoWriteLN('------------------');
    DoWriteLN('Startup Switches');
    DoWriteLN('-h, --help          Status Information and help.');
    DoWriteLN('-f, --force         Forces bouncer to run even if Wanted PID is in use');
    DoWriteLN('-n, --no-fork       Do not attempt to fork');
    DoWriteLN('-s, --serv          Run in ServServ Mode (Default: GameServ)');
    DoWriteLN('-i, --id <id>       Server ID to use when running as GameServ (Defaults to the first in the database)');
    DoWriteLN('-c, --conf <file>   File to use for mysql details (Default: ./mysql.conf)');
    DoWriteLN('------------------');
    exit;
  end;
  InitDebug();
  AddDebug(DEBUG_PROC,'Main Application Handler');
  Config.StartTime := unixtime;
  DoWriteLN('------------------');
  DoWriteLN('  dWOL  Started.  ');
  DoWriteLN('------------------');
  if (CLI.Exists('s') <> -1) Or (CLI.Exists('serv') <> -1) then begin
    ServServer := true;
    DoWriteLN('[dWOL] Running in ServServ mode.');
  end
  else begin
    DoWriteLN('[dWOL] Running in GameServ mode.');  
  end;
  DoWriteln('[dWOL] Initialising Linked Lists.');
  LL_InitSockets();
  LL_InitClanChan();  
  Config.ServServ := ServServer;
  DoWriteln('[dWOL] Loading Config File.');
  ParseDataFile();  
  DoWriteln('[dWOL] Done. Calling DoStuff');
  DoStuff();
end.
