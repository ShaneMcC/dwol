{*
 * ControlSocket - Maintains the socket used for listening for clients
 * Copyright (C) 2005 Shane "Dataforce" Mc Cormack
 * For conditions of distribution and use, see copyright notice in license.txt
 *
 * SVN: $Id$
 *}
unit ControlSocket;

{$I defines.inc}

interface

uses
  {$ifdef win32}WSocket, WCore{$else}lsocket, lcore{$endif}, DebugList, SysUtils,
  UserSocket, svn, ipstrings;

type
  TListen = class
    constructor Create();
    private
      Sock: TWSocket;

      procedure SessionConnected(sender : tobject; error : word);
      procedure SessionAvail(sender : tobject; error : word);
    public
      function Bind(Host: String; Port: Integer): boolean;
  end;

implementation
uses Common;

constructor TListen.create();
begin
  AddDebug(DEBUG_PROC,'TListen.Create');
  DoWriteLN('[TListen.Create] Started');
  Sock := TWSocket.Create(nil);
  Sock.Proto := 'TCP';
  Sock.OnSessionAvailable := SessionAvail;
  Sock.OnSessionConnected := SessionConnected;
  DoWriteLN('[TListen.Create] Create Complete.');
end;

function TListen.Bind(Host: String; Port: Integer): boolean;
begin
  AddDebug(DEBUG_PROC,'TListen.Bind');
  DoWriteLN('[TListen.Bind] Started.');
  try
    Sock.Close;
    DoWriteLN('[TListen.Bind] Socket Closed');
    Sock.Addr := Host;
    Sock.Port := inttostr(Port);
    DoWriteLN('[TListen.Bind] Listen on '+Host+':'+inttostr(Port));
    Sock.Listen;
    DoWriteLN('[TListen.Bind] Bind Complete.');
    result := true;
  except
    result := false;
    DoWriteLN('[TListen.Bind] Bind Failed.');
  end;
end;

procedure TListen.SessionConnected(sender : tobject; error : word);
begin
  AddDebug(DEBUG_PROC,'TListen.SessionConnected');
 // Nothing Special;
 DoWriteln('[TListen.SessionConnected] Session Connected');
end;

procedure TListen.SessionAvail(sender : tobject; error : word);
var
  ptr: TWOL_Socket;
begin
  AddDebug(DEBUG_PROC,'TListen.SessionAvail');
  DoWriteLN('[TListen.SessionAvail] Session Availible. Creating Socket.');
  ptr := LL_AddSocket();
  ptr.sock.Dup(TSocket(Sender).Accept);
  ptr.IPInt := ipstrtoint(ptr.sock.GetPeerAddr);

  DoWriteln('[TListen.SessionAvail] Socket Created.');
  ptr.Timer.enabled := true;
end;

{---- Adjust global SVN revision ----}
initialization
  SVNRevision('$Id$');
end.
