{ Copyright (C) 2005 Bas Steendijk and Peter Green
  For conditions of distribution and use, see copyright notice in zlib_license.txt
  which is included in the package
  ----------------------------------------------------------------------------- }
unit dnsasync;

interface

uses
  {$ifdef win32}wcore,wsocket,{$else}lsocket,lcore,{$endif}
  classes,binipstuff,dnscore,btime;

{$ifdef win32}
type
  TSocketevent = procedure(Sender: TObject; Error: word) of object;
{$endif}

type
  tdnsasync=class(tcomponent)
  public
    sock:twsocket;
    addr,port:string;
    sockopen:boolean;
    onrequestdone:tsocketevent;
    state:tdnsstate;
    forwardfamily:integer;
    dnsserverid:integer;
    startts:double;
    {$ifdef win32}
    procedure apilookupdonehandler(sender:tobject;error:word);
    {$endif}
    procedure cancel;
    function dnsresult:string;
    procedure dnsresultbin(var binip:tbinip);
    procedure asyncprocess;
    procedure receivehandler(sender:tobject;error:word);
    function sendquery(const packet:tdnspacket;len:integer):boolean;
    procedure forwardlookup(const name:string);
    procedure reverselookup(const binip:tbinip);
    constructor create(aowner:tcomponent); override;
    destructor destroy; override;
  end;

implementation

uses sysutils;

constructor tdnsasync.create;
begin
  inherited create(aowner);
  dnsserverid := -1;
  sock := twsocket.create(self);
end;

destructor tdnsasync.destroy;
begin
  if dnsserverid >= 0 then begin
    reportlag(dnsserverid,-1);
    dnsserverid := -1;
  end;
  sock.release;
  setstate_request_init('',state);
  inherited destroy;
end;

{$ifdef win32}
procedure tdnsasync.apilookupdonehandler;
var
  a:integer;
begin
  a := twsocket(sender).dnsresultlist.count;
  if a > 0 then begin
    if state.requesttype = querytype_ptr then begin
      state.resultstr := sock.dnsresult;
      fillchar(state.resultbin,sizeof(state.resultbin),0);
    end else begin
      ipstrtobin(sock.DnsResult,state.resultbin);
      state.resultstr := '';
      state.resultaction := action_done;
    end;
  end else begin
    setstate_failure(state);
  end;
  onrequestdone(self,0);
end;
{$endif}

procedure tdnsasync.receivehandler;
begin
  if dnsserverid >= 0 then begin
    reportlag(dnsserverid,trunc((unixtimefloat-startts)*1000));
    dnsserverid := -1;
  end;
{  writeln('received reply');}
  fillchar(state.packet,sizeof(state.packet),0);
  state.packetlen := twsocket(sender).Receive(@state.packet, SizeOf(state.packet));
  state.parsepacket := true;
  asyncprocess;
end;

function tdnsasync.sendquery;
begin
{  writeln('sendquery ',decodename(state.packet,state.packetlen,12,0,a),' ',state.requesttype);}
  result := false;
  if len = 0 then exit; {no packet}
  if not sockopen then begin
    if addr <> '' then sock.addr := addr else sock.addr := getcurrentsystemnameserver(dnsserverid);//getunixnameservercached;
    startts := unixtimefloat;
    if port = '' then port := '53';
    sock.port := port;
    sock.Proto := 'udp';
    sock.ondataavailable := receivehandler;
    try
      sock.connect;
    except
      on e:exception do begin
        writeln('exception '+e.message);
        exit;
      end;
    end;
    sockopen := true;
  end;
  sock.send(@packet,len);
  result := true;
end;

procedure tdnsasync.asyncprocess;
begin
  state_process(state);
  case state.resultaction of
    action_ignore: begin {do nothing} end;
    action_done: begin
      onrequestdone(self,0);
    end;
    action_sendquery:begin
      sendquery(state.packet,state.packetlen);
    end;
  end;
end;

procedure tdnsasync.forwardlookup;
begin
  ipstrtobin(name,state.resultbin);
  if state.resultbin.family <> 0 then begin
    onrequestdone(self,0);
    exit;
  end;

{$ifdef win32}
  if addr = '' then begin
    {do windows API dns lookup if no dns server is specified}
    state.requesttype := querytype_a;
    sock.OnDnsLookupDone := apilookupdonehandler;
    sock.dnslookup(name);
    exit;
  end;
{$endif}

  setstate_forward(name,state,forwardfamily);
  asyncprocess;
end;

procedure tdnsasync.reverselookup;
begin
{$ifdef win32}
  if addr = '' then begin
    {do windows API dns lookup if no dns server is specified}
    state.requesttype := querytype_ptr;
    sock.OnDnsLookupDone := apilookupdonehandler;
    sock.reversednslookup(ipbintostr(binip));
    exit;
  end;
{$endif}

  setstate_reverse(binip,state);
  asyncprocess;
end;

function tdnsasync.dnsresult;
begin
  if state.resultstr <> '' then result := state.resultstr else begin
    result := ipbintostr(state.resultbin);
  end;
end;

procedure tdnsasync.dnsresultbin(var binip:tbinip);
begin
  move(state.resultbin,binip,sizeof(binip));
end;

procedure tdnsasync.cancel;
begin
  if dnsserverid >= 0 then begin
    reportlag(dnsserverid,-1);
    dnsserverid := -1;
  end;
  if sockopen then begin
    sock.close;
    sockopen := false;
  end;
  setstate_failure(state);
  onrequestdone(self,0);
end;

end.
