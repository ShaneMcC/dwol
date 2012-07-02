{lsocket.pas}

{socket code by plugwash}

{ Copyright (C) 2005 Bas Steendijk and Peter Green
  For conditions of distribution and use, see copyright notice in zlib_license.txt
  which is included in the package
  ----------------------------------------------------------------------------- }
{
changes by plugwash (20030728)
* created handlefdtrigger virtual method in tlasio (overridden in tlsocket) and moved a lot of code from messageloop into it
* changed tlasio to tlasio
* split fdhandle into fdhandlein and fdhandleout
* i now use fdsrmaster and fdswmaster instead of rebuilding the lists every loop
* split lsocket.pas into lsocket.pas and lcore.pas


changes by beware (20030903)
* added getxaddr, getxport (local addr, port, as string)
* added getpeername, remote addr+port as binary
* added htons and htonl functions (endian swap, same interface as windows API)

beware (20030905)
* if connect failed (conn refused) set state to connected and call internalclose, to get closed handler (instead of fdclose)
* (lcore) if closing the fd's in internalcose, set fd's to -1 because closing an fd makes it invalid

beware (20030927)
* fixed: on connect failed, tried to close fdhandle's which were already set to -1, added check

beware (20031017)
* added getpeeraddr, getpeerport, remote addr+port as string
}


unit lsocket;
{$mode delphi}
interface
  uses
    sysutils,
    {$ifdef VER1_0}
      linux,
    {$else}
      baseunix,unix,
    {$endif}
    sockets,classes,pgdebugout,pgtypes,lcore,fd_utils,binipstuff,dnssync;
type
  sunB = packed record
    s_b1, s_b2, s_b3, s_b4: byte;
  end;

  SunW = packed record
    s_w1, s_w2: word;
  end;

  TInAddr = packed record
    case integer of
      0: (S_un_b: SunB);
      1: (S_un_w: SunW);
      2: (S_addr: cardinal);
  end;
  {$ifdef ipv6}
    {$ifdef ver1_0}
      cuint16=word;
      cuint32=dword;
      sa_family_t=word;


      TInetSockAddr6 = packed Record
        sin6_family   : sa_family_t;
        sin6_port     : cuint16;
        sin6_flowinfo : cuint32;
        sin6_addr     : Tin6_addr;
        sin6_scope_id : cuint32;
      end;
    {$endif}
  {$endif}
  TinetSockAddrv = packed record
    case integer of
      0: (InAddr:TInetSockAddr);
      {$ifdef ipv6}
      1: (InAddr6:TInetSockAddr6);
      {$endif}
  end;

  type
    tsockaddrin=TInetSockAddr;

  type
    TLsocket = class(tlasio)
    public
      //a: string;

      inAddr             : TInetSockAddrV;
{      inAddrSize:integer;}

      //host               : THostentry      ;

      //mainthread         : boolean         ; //for debuggin only
      addr:string;
      port:string;
      localaddr:string;
      localport:string;
      proto:string;
      udp:boolean;
      listenqueue:integer;
      function getaddrsize:integer;
      procedure connect; virtual;
      procedure bindsocket;
      procedure listen;
      function accept : longint;
      function sendto(dest:TInetSockAddr;destlen:integer;data:pointer;len:integer):integer; virtual;
      function receivefrom(data:pointer;len:integer;var src:TInetSockAddr;var srclen:integer):integer; virtual;
      //procedure internalclose(error:word);override;
      procedure handlefdtrigger(readtrigger,writetrigger:boolean); override;
      function send(data:pointer;len:integer):integer;override;
      procedure sendstr(const str : string);override;
      function Receive(Buf:Pointer;BufSize:integer):integer; override;
      function getpeername(var addr:tsockaddrin;addrlen:integer):integer; virtual;
      procedure getXaddrbin(var binip:tbinip); virtual;
      procedure getpeeraddrbin(var binip:tbinip); virtual;
      function getXaddr:string; virtual;
      function getpeeraddr:string; virtual;
      function getXport:string; virtual;
      function getpeerport:string; virtual;
      constructor Create(AOwner: TComponent); override;
    end;
    tsocket=longint; // for compatibility with twsocket

  twsocket=tlsocket; {easy}

function htons(w:word):word;
function htonl(i:integer):integer;
{!!!function longipdns(s:string):longint;}

{$ifdef ipv6}
const
  v4listendefault:boolean=false;
{$endif}


const
  TCP_NODELAY=1;
  IPPROTO_TCP=6;

implementation
{$include unixstuff.inc}

function longip(s:string):longint;inline;
var
  l:longint;
  a,b:integer;

function convertbyte(const s:string):integer;inline;
begin
  result := strtointdef(s,-1);
  if result < 0 then exit;
  if result > 255 then exit;

  {01 exception}
  if (result <> 0) and (s[1] = '0') then begin
    result := -1;
    exit;
  end;

  {+1 exception}
  if not (s[1] in ['0'..'9']) then begin
    result := -1;
    exit
  end;
end;

begin
  result := 0;
  a := pos('.',s);
  if a = 0 then exit;
  b := convertbyte(copy(s,1,a-1));if (b < 0) then exit;
  l := b shl 24;
  s := copy(s,a+1,256);
  a := pos('.',s);
  if a = 0 then exit;
  b := convertbyte(copy(s,1,a-1));if (b < 0) then exit;
  l := l or b shl 16;
  s := copy(s,a+1,256);
  a := pos('.',s);
  if a = 0 then exit;
  b := convertbyte(copy(s,1,a-1));if (b < 0) then exit;
  l := l or b shl 8;
  s := copy(s,a+1,256);
  b := convertbyte(copy(s,1,256));if (b < 0) then exit;
  l := l or b;
  result := l;
end;

(*!!!
function longipdns(s:string):longint;
var
  host : thostentry;
begin
  if s = '0.0.0.0' then begin
    result := 0;
  end else begin
    result := longip(s);
    if result = 0 then begin
      if gethostbyname(s,host) then begin;
        result := htonl(Longint(Host.Addr));
      end;
      //writeln(inttohex(longint(host.addr),8))
    end;
    if result = 0 then begin
      if resolvehostbyname(s,host) then begin;
        result := htonl(Longint(Host.Addr));
      end;
      //writeln(inttohex(longint(host.addr),8))
    end;
  end;
end;
*)


function htons(w:word):word;
begin
  {$ifdef ENDIAN_LITTLE}
  result := ((w and $ff00) shr 8) or ((w and $ff) shl 8);
  {$else}
  result := w;
  {$endif}
end;

function htonl(i:integer):integer;
begin
  {$ifdef ENDIAN_LITTLE}
  result := (i shr 24) or (i shr 8 and $ff00) or (i shl 8 and $ff0000) or (i shl 24 and $ff000000);
  {$else}
  result := i;
  {$endif}
end;

function tlsocket.getaddrsize:integer;
begin
  {$ifdef ipv6}
  if inaddr.inaddr.family = AF_INET6 then result := sizeof(tinetsockaddr6) else
  {$endif}
  result := sizeof(tinetsockaddr);
end;

function makeinaddrv(addr,port:string;var inaddr:tinetsockaddrv):integer;
var
  biniptemp:tbinip;
begin
  result := 0;
  biniptemp := forwardlookup(addr,10);
  fillchar(inaddr,sizeof(inaddr),0);
  if biniptemp.family = AF_INET then begin
    inAddr.InAddr.family:=AF_INET;
    inAddr.InAddr.port:=htons(strtointdef(port,0));
    inAddr.InAddr.addr:=biniptemp.ip;
    result := sizeof(tinetsockaddr);
  end else
  {$ifdef ipv6}
  if biniptemp.family = AF_INET6 then begin
    inAddr.InAddr6.sin6_family:=AF_INET6;
    inAddr.InAddr6.sin6_port:=htons(strtointdef(port,0));
    inAddr.InAddr6.sin6_addr:=biniptemp.ip6;
    result := sizeof(tinetsockaddr6);
  end else
  {$endif}
  raise esocketexception.create('unable to resolve address: '+addr);
end;

procedure tlsocket.connect;
var
  a:integer;
begin
  if state <> wsclosed then close;
  //prevtime := 0;
  makeinaddrv(addr,port,inaddr);

  udp := uppercase(proto) = 'UDP';
  if udp then a := SOCK_DGRAM else a := SOCK_STREAM;
  a := Socket(inaddr.inaddr.family,a,0);

  //writeln(ord(inaddr.inaddr.family));
  if a = -1 then begin
    lasterror := socketerror;
    raise esocketexception.create('unable to create socket');
  end;
  try
    dup(a);
    bindsocket;
    if udp then begin
      SetSocketOptions(fdhandleout, SOL_SOCKET, SO_BROADCAST, 'TRUE', Length('TRUE'));
      state := wsconnected;
      if assigned(onsessionconnected) then onsessionconnected(self,0);
    end else begin
      state :=wsconnecting;
      sockets.Connect(fdhandlein,inADDR,getaddrsize);
    end;
    rmasterset(fdhandlein);
    if udp then begin
      wmasterclr(fdhandleout);
    end else begin
      wmasterset(fdhandleout);
    end;
    //sendq := '';
  except
    on e: exception do begin
      fdcleanup;
      raise; //reraise the exception
    end;
  end;
end;

procedure tlsocket.sendstr(const str : string);
begin
  if udp then begin
    send(@str[1],length(str))
  end else begin
    inherited sendstr(str);
  end;
end;

function tlsocket.send(data:pointer;len:integer):integer;
begin
  if udp then begin
    //writeln('sending to '+inttohex(inaddr.inaddr.addr,8));
    result := sendto(inaddr.inaddr,getaddrsize,data,len)
;
    //writeln('send result',result);
    //writeln('errno',errno);
  end else begin
    result := inherited send(data,len);
  end;
end;


function tlsocket.receive(Buf:Pointer;BufSize:integer):integer;
begin
  if udp then begin
    result := fdread(self.fdhandlein,buf^,bufsize);
  end else begin
    result := inherited receive(buf,bufsize);
  end;
end;

procedure tlsocket.bindsocket;
var
  inAddrtemp:TInetSockAddrV;
  inaddrtempsize:integer;
begin
  try
    if (localaddr <> '') or (localport <> '') then begin
      if localaddr = '' then begin
        {$ifdef ipv6}
        if inaddr.inaddr.family = AF_INET6 then localaddr := '::' else
        {$endif}
        localaddr := '0.0.0.0';
      end;
      //gethostbyname(localaddr,host);

      inaddrtempsize := makeinaddrv(localaddr,localport,inaddrtemp);

      If Not Bind(fdhandlein,inaddrtemp,inaddrtempsize) Then begin
        state := wsclosed;
        lasterror := socketerror;
        raise ESocketException.create('unable to bind, error '+inttostr(lasterror));
      end;
      state := wsbound;
    end;
  except
    on e: exception do begin
      fdcleanup;
      raise; //reraise the exception
    end;
  end;
end;

procedure tlsocket.listen;
var
  yes:longint;
  socktype:integer;
  biniptemp:tbinip;
begin
  if state <> wsclosed then close;
  udp := uppercase(proto) = 'UDP';
  if udp then socktype := SOCK_DGRAM else socktype := SOCK_STREAM;

  if addr = '' then begin
    {$ifdef ipv6}
    if not v4listendefault then begin
      addr := '::';
    end else
    {$endif}
    addr := '0.0.0.0';
  end;
  biniptemp := forwardlookup(addr,10);
  addr := ipbintostr(biniptemp);
  fdhandlein := socket(biniptemp.family,socktype,0);
  {$ifdef ipv6}
  if (addr = '::') and (origaddr = '') and (fdhandlein < 0) then begin
    addr := '0.0.0.0';
    fdhandlein := socket(AF_INET,socktype,0);
  end;
  {$endif}
  if fdhandlein = -1 then raise ESocketException.create('unable to create socket');
  dup(fdhandlein); // sets up maxs and copies handle to fdhandleout among other things
  fdreverse[fdhandlein] := self;
  state := wsclosed; // then set this back as it was an undesired side effect of dup

  try
    yes := $01010101;  {Copied this from existing code. Value is empiric,
                    but works. (yes=true<>0) }
    if SetSocketOptions(fdhandlein, SOL_SOCKET, SO_REUSEADDR,yes,sizeof(yes))=-1 then begin
      raise ESocketException.create('unable to set socket options');
    end;

    localaddr := addr;
    localport := port;
    bindsocket;

    if not udp then begin
      {!!! allow custom queue length? default 5}
      if listenqueue = 0 then listenqueue := 5;
      If Not sockets.Listen(fdhandlein,listenqueue) Then raise esocketexception.create('unable to listen');
      state := wsListening;
    end else begin
      SetSocketOptions(fdhandleout, SOL_SOCKET, SO_BROADCAST, 'TRUE', Length('TRUE'));
      state := wsconnected;
    end;
  finally
    if state = wsclosed then begin
      if fdhandlein >= 0 then begin
        {one *can* get here without fd -beware}
        rmasterclr(fdhandlein);
        fdclose(fdhandlein); // we musnt leak file discriptors
        fdreverse[fdhandlein] := nil;
        fdhandlein := -1;
      end;
    end else begin
      rmasterset(fdhandlein);
    end;
    if fdhandleout >= 0 then wmasterclr(fdhandleout);
  end;
end;

function tlsocket.accept : longint;
var
  FromAddrSize     : LongInt;        // i don't realy know what to do with these at this
  FromAddr         : TInetSockAddr;  // at this point time will tell :)
begin
  rmasterset(fdhandlein);
  FromAddrSize := Sizeof(FromAddr);
  result := sockets.accept(fdhandlein,fromaddr,fromaddrsize);
  if result = -1 then raise esocketexception.create('error '+inttostr(socketerror)+' while accepting');
  if result > absoloutemaxs then begin
    fdclose(result);
    result := -1;
    raise esocketexception.create('file discriptor out of range');
  end;
end;

function tlsocket.sendto;
begin
  result := sockets.sendto(self.fdhandleout,data^,len,0,dest,destlen);
end;

function tlsocket.receivefrom;
begin
  result := sockets.recvfrom(self.fdhandlein,data^,len,0,src,srclen);
end;

procedure tlsocket.handlefdtrigger(readtrigger,writetrigger:boolean);
var
  tempbuf:array[0..receivebufsize-1] of byte;
begin
  if (state =wslistening) and readtrigger then begin
{    debugout('listening socket triggered on read');}
    rmasterclr(fdhandlein);
    if assigned(onsessionAvailable) then onsessionAvailable(self,0);
  end;
  if udp and readtrigger then begin
    if assigned(ondataAvailable) then ondataAvailable(self,0);
    {!!!test}
    exit;
  end;
  if (state =wsconnecting) and writetrigger then begin
    // code for dealing with the reults of a non-blocking connect is
    // rather complex
    // if just write is triggered it means connect suceeded
    // if both read and write are suceededed it can mean 2 things
    // 1: connect ok and data availible
    // 2: connect fail
    // to find out which you must read from the socket and look for errors
    // there if we read successfully we drop through into the code for fireing
    // the read event
    if not readtrigger then begin
      state := wsconnected;
      if assigned(onsessionconnected) then onsessionconnected(self,0);
    end else begin
      numread := fdread(fdhandlein,tempbuf,sizeof(tempbuf));
      if numread <> -1 then begin
        state := wsconnected;
        if assigned(onsessionconnected) then onsessionconnected(self,0);
        //connectread := true;
        recvq.add(@tempbuf,numread);
      end else begin
        state := wsconnected;
        if assigned(onsessionconnected) then onsessionconnected(self,linuxerror);
{        debugout('connect fail');}
        self.internalclose(0);
        recvq.del(maxlongint);
      end;
      // if things went well here we are now in the state wsconnected with data sitting in our receive buffer
      // so we drop down into the processing for data availible
    end;
    if fdhandlein >= 0 then begin
      if state = wsconnected then begin
        rmasterset(fdhandlein);
      end else begin
        rmasterclr(fdhandlein);
      end;
    end;
    if fdhandleout >= 0 then begin
      if sendq.size = 0 then begin
        //don't clear the bit in fdswmaster if data is in the sendq
        wmasterclr(fdhandleout);
      end;
    end;

  end;
  inherited handlefdtrigger(readtrigger,writetrigger);
end;

constructor tlsocket.Create(AOwner: TComponent);
begin
  inherited create(aowner);
  closehandles := true;
end;


function tlsocket.getpeername(var addr:tsockaddrin;addrlen:integer):integer;
begin
  result := sockets.getpeername(self.fdhandlein,addr,addrlen);
end;

procedure tlsocket.getxaddrbin(var binip:tbinip);
var
  addr:tinetsockaddrv;
  i:integer;
begin
  i := sizeof(addr);
  fillchar(addr,sizeof(addr),0);
  sockets.getsocketname(self.fdhandlein,addr,i);

  binip.family := addr.inaddr.family;
  {$ifdef ipv6}
  if addr.inaddr6.sin6_family = AF_INET6 then begin
    binip.ip6 := addr.inaddr6.sin6_addr;
  end else
  {$endif}
  begin
    binip.ip := addr.inaddr.addr;
  end;
  converttov4(binip);
end;

procedure tlsocket.getpeeraddrbin(var binip:tbinip);
var
  addr:tinetsockaddrv;
  i:integer;
begin
  i := sizeof(addr);
  fillchar(addr,sizeof(addr),0);
  sockets.getpeername(self.fdhandlein,addr,i);

  binip.family := addr.inaddr.family;
  {$ifdef ipv6}
  if addr.inaddr6.sin6_family = AF_INET6 then begin
    binip.ip6 := addr.inaddr6.sin6_addr;
  end else
  {$endif}
  begin
    binip.ip := addr.inaddr.addr;
  end;
  converttov4(binip);
end;

function tlsocket.getXaddr:string;
var
  biniptemp:tbinip;
begin
  getxaddrbin(biniptemp);
  result := ipbintostr(biniptemp);
  if result = '' then result := 'error';
end;

function tlsocket.getpeeraddr:string;
var
  biniptemp:tbinip;
begin
  getpeeraddrbin(biniptemp);
  result := ipbintostr(biniptemp);
  if result = '' then result := 'error';
end;

function tlsocket.getXport:string;
var
  addr:tinetsockaddr;
  i:integer;
begin
  i := sizeof(addr);
  sockets.getsocketname(self.fdhandlein,addr,i);
  i := htons(addr.port);
  result := inttostr(i);
end;

function tlsocket.getpeerport:string;
var
  addr:tinetsockaddr;
  i:integer;
begin
  i := sizeof(addr);
  sockets.getpeername(self.fdhandlein,addr,i);
  i := htons(addr.port);
  result := inttostr(i);
end;

end.

