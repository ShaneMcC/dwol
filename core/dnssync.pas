{ Copyright (C) 2005 Bas Steendijk and Peter Green
  For conditions of distribution and use, see copyright notice in zlib_license.txt
  which is included in the package
  ----------------------------------------------------------------------------- }
unit dnssync;
{$ifdef fpc}
  {$mode delphi}
{$endif}

interface
  uses
    dnscore,
    binipstuff,
    {$ifdef win32}
      winsock,
      windows,
    {$else}
      {$ifdef VER1_0}
        linux,
      {$else}
        baseunix,unix,
      {$endif}
      sockets,
      fd_utils,
    {$endif}
    sysutils;
// note: timeout is in seconds
  function forwardlookup(name:string;timeout:integer):tbinip;

var
  dnssyncserver:string;
  id : integer;
  {$ifdef win32}
    sendquerytime : integer;
  {$else}
    sendquerytime : ttimeval;
  {$endif}
implementation

{$i unixstuff.inc}
{$i ltimevalstuff.inc}

var
  fd:integer;
  state:tdnsstate;
{$ifdef win32}
  const
    winsocket = 'wsock32.dll';
  function sendto(s: TSocket; const Buf; len, flags: Integer; var addrto: TinetSockAddr; tolen: Integer): Integer; stdcall; external    winsocket name 'sendto';
  function bind(s: TSocket; var addr: TinetSockAddr; namelen: Integer): Longbool; stdcall; external    winsocket name 'bind';
  type
    fdset=tfdset;
{$endif}

function sendquery(const packet:tdnspacket;len:integer):boolean;
var
  addr       : string;
  port       : string;
  inaddr     : TInetSockAddr;

begin
{  writeln('sendquery ',decodename(state.packet,state.packetlen,12,0,a),' ',state.requesttype);}
  result := false;
  if len = 0 then exit; {no packet}

  if dnssyncserver <> '' then addr := dnssyncserver else addr := getcurrentsystemnameserver(id);
  port := '53';

  inAddr.family:=AF_INET;
  inAddr.port:=htons(strtointdef(port,0));
  inAddr.addr:=htonl(longip(addr));

  sendto(fd,packet,len,0,inaddr,sizeof(inaddr));
  {$ifdef win32}
    sendquerytime := GetTickCount and $3fff;
  {$else}
    gettimeofday(sendquerytime);
  {$endif}
  result := true;
end;

procedure setupsocket;
var
  inAddrtemp : TInetSockAddr;
begin
  if fd > 0 then exit;

  fd := Socket(AF_INET,SOCK_DGRAM,0);
  inAddrtemp.family:=AF_INET;
  inAddrtemp.port:=0;
  inAddrtemp.addr:=0;{htonl(longip('0.0.0.0'));}
  If {$ifndef win32}Not{$endif} Bind(fd,inAddrtemp,SizeOf(inAddrtemp)) Then begin
    {$ifdef win32}
      raise Exception.create('unable to bind '+inttostr(WSAGetLastError));
    {$else}
      raise Exception.create('unable to bind '+inttostr(socketError));
    {$endif}
  end;
end;

procedure resolveloop(timeout:integer);
var
  selectresult   : integer;
  fds            : fdset;
  {$ifdef win32}
    endtime      : longint;
    starttime    : longint;
    wrapmode     : boolean;
    currenttime  : integer;
  {$else}
    endtime      : ttimeval;
    currenttime    : ttimeval;

  {$endif}
  lag            : ttimeval;
  selecttimeout	 : ttimeval;


begin
  {$ifdef win32}
    starttime := GetTickCount and $3fff;
    endtime := starttime +(timeout*1000);
    if (endtime and $4000)=0 then begin
      wrapmode := false;
    end else begin
      wrapmode := true;
    end;
    endtime := endtime and $3fff;
  {$else}
    gettimeofday(endtime);
    endtime.tv_sec := endtime.tv_sec + timeout;
  {$endif}

  setupsocket;
  repeat
    state_process(state);
    case state.resultaction of
      action_ignore: begin
{        writeln('ignore');}
        {do nothing}
      end;
      action_done: begin
{        writeln('done');}
        exit;
        //onrequestdone(self,0);
      end;
      action_sendquery:begin
{        writeln('send query');}
        sendquery(state.packet,state.packetlen);
      end;
    end;
    {$ifdef win32}
      currenttime := GetTickCount and $3fff;
      msectotimeval(selecttimeout, (endtime-currenttime)and$3fff);
    {$else}
      gettimeofday(currenttime);
      selecttimeout := endtime;
      tv_substract(selecttimeout,currenttime);
    {$endif}
    fd_zero(fds);
    fd_set(fd,fds);
    if (selecttimeout.tv_sec > 0) or (selecttimeout.tv_usec > retryafter) then begin
      selecttimeout.tv_sec := 0;
      selecttimeout.tv_usec := retryafter;
    end;
    selectresult := select(fd+1,@fds,nil,nil,@selecttimeout);
    if selectresult > 0 then begin
{      writeln('selectresult>0');}
      fillchar(state.packet,sizeof(state.packet),0);
      {$ifdef win32}
        lag := msectotimeval((currenttime-sendquerytime)and$3fff);
      {$else}
        lag := currenttime;
        tv_substract(lag,sendquerytime);

      {$endif}

      reportlag(id,(lag.tv_sec*1000000)+lag.tv_usec);
      state.packetlen := recv(fd,state.packet, SizeOf(state.packet),0);
      state.parsepacket := true;
    end;
    if selectresult < 0 then exit;
    if selectresult = 0 then begin
      {$ifdef win32}
        currenttime := GetTickCount;
      {$else}
        gettimeofday(currenttime);
      {$endif}
      reportlag(id,-1);
      if {$ifdef win32}(currenttime >= endtime)and ((not wrapmode) or (currenttime < starttime)) {$else}tv_compare(currenttime,endtime){$endif} {currenttime >= endtime } then begin
        exit;
      end else begin
        //resend
        sendquery(state.packet,state.packetlen);
      end;
    end;
  until false;
end;

function forwardlookup(name:string;timeout:integer):tbinip;
begin
  ipstrtobin(name,result);
  if result.family <> 0 then exit;

  setstate_forward(name,state,0);
  resolveloop(timeout);
  result := state.resultbin;
end;
{$ifdef win32}
  var
    wsadata : twsadata;

  initialization
    WSAStartUp($2,wsadata);
  finalization
    WSACleanUp;
{$endif}
end.


