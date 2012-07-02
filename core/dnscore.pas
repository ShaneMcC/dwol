{ Copyright (C) 2005 Bas Steendijk and Peter Green
  For conditions of distribution and use, see copyright notice in zlib_license.txt
  which is included in the package
  ----------------------------------------------------------------------------- }
      
unit dnscore;

{$ifdef fpc}{$mode delphi}{$endif}

{
- all words are network order

www.google.com A request:

0, 2: random transaction ID
2, 2: flags: only the "recursion desired" bit set. (bit 8 of word)
4, 2: questions: 1
6, 2: answer RR's: 0.
8, 2: authority RR's: 0.
10, 2: additional RR's: 0.
12, n: payload:
  query:
    #03 "www" #06 "google" #03 "com" #00
    size-4, 2: type: host address (1)
    size-2, 2: class: inet (1)

reply:

0,2: random transaction ID
2,2: flags: set: response (bit 15), recursion desired (8), recursion available (7)
4,4: questions: 1
6,4: answer RR's: 2
8,4: authority RR's: 9
10,4: additional RR's: 9
12: payload:
  query:
    ....
  answer: CNAME
    0,2 "c0 0c" "name: www.google.com"
    2,2 "00 05" "type: cname for an alias"
    4,2 "00 01" "class: inet"
    6,4: TTL
    10,2: data length "00 17" (23)
    12: the cname name (www.google.akadns.net)
  answer: A
    0,2 ..
    2,2 "00 01" host address
    4,2 ...
    6,4 ...
    10,2: data length (4)
    12,4: binary IP
  authority - 9 records
  additional - 9 records


  ipv6 AAAA reply:
    0,2: ...
    2,2: type: 001c
    4,2: class: inet (0001)
    6,2: TTL
    10,2: data size (16)
    12,16: binary IP

  ptr request: query type 000c

name compression: word "cxxx" in the name, xxx points to offset in the packet}



interface

uses binipstuff,classes;

const
  maxnamelength=127;
  maxnamefieldlen=63;
  //note: when using action_ignore the dnscore code *must* preserve the contents of state.packet to allow for retries
  //note: action_ignore must not be used in response to the original request but there is no valid reason for doing this anyway
  action_ignore=0;
  action_done=1;
  action_sendquery=2;
  querytype_a=1;
  querytype_cname=5;
  querytype_aaaa=28;
  querytype_ptr=12;
  querytype_ns=2;
  querytype_soa=6;
  querytype_mx=15;

  maxrecursion=10;
  maxrrofakind=20;
  
  retryafter=300000; //microseconds must be less than one second;
  timeoutlag=1000000000; // penalty value to be treated as lag in the event of a timeout (microseconds)
type
  dvar=array[0..0] of byte;
  pdvar=^dvar;
  tdnspacket=packed record
    id:word;
    flags:word;
    rrcount:array[0..3] of word;
    payload:array[0..511-12] of byte;
  end;

  tdnsstate=record
    id:word;
    recursioncount:integer;
    queryname:string;
    requesttype:word;
    parsepacket:boolean;
    resultstr:string;
    resultbin:tbinip;
    resultaction:integer;
    numrr1:array[0..3] of integer;
    numrr2:integer;
    rrdata:string;
    packetlen:integer;
    packet:tdnspacket;
    forwardfamily:integer;
  end;

  trr=packed record
    requesttypehi:byte;
    requesttype:byte;
    clas:word;
    ttl:integer;
    datalen:word;
    data:array[0..511] of byte;
  end;

  trrpointer=packed record
    p:pointer;
    ofs:integer;
    len:integer;
    namelen:integer;
  end;

function buildrequest(const name:string;var packet:tdnspacket;requesttype:word):integer;
function makereversename(const binip:tbinip):string;

procedure setstate_request_init(const name:string;var state:tdnsstate);
procedure setstate_forward(const name:string;var state:tdnsstate;family:integer);
procedure setstate_reverse(const binip:tbinip;var state:tdnsstate);
procedure setstate_failure(var state:tdnsstate);
procedure setstate_return(const rrp:trrpointer;len:integer;var state:tdnsstate);

procedure state_process(var state:tdnsstate);

function decodename(const packet:tdnspacket;len,start,recursion:integer;var numread:integer):string;

var randomfunction:function:integer;


procedure populatednsserverlist;
procedure cleardnsservercache;

var
  dnsserverlist : tstringlist;
//  currentdnsserverno : integer;

function getcurrentsystemnameserver(var id:integer) :string;

//var
//  unixnameservercache:string;
{ $endif}


procedure reportlag(id:integer;lag:integer); //lag should be in microseconds and should be -1 to report a timeout
var
  failurereason:string;

implementation

uses
  {$ifdef win32}
    windows,
  {$endif}

  sysutils;

function buildrequest(const name:string;var packet:tdnspacket;requesttype:word):integer;
var
  a,b:integer;
  s:string;
  arr:array[0..sizeof(packet)-1] of byte absolute packet;
begin
 { writeln('buildrequest: name: ',name);}
  result := 0;
  fillchar(packet,sizeof(packet),0);
  if assigned(randomfunction) then packet.id := (randomfunction and $ffff) else packet.id := random(65536);
  packet.flags := htons($0100);
  packet.rrcount[0] := htons($0001);


  s := copy(name,1,maxnamelength);
  if s = '' then exit;
  if s[length(s)] <> '.' then s := s + '.';
  b := 0;
  {encode name}
  if (s = '.') then begin
    packet.payload[0] := 0;
    result := 12+5;
  end else begin
    for a := 1 to length(s) do begin
      if s[a] = '.' then begin
        if b > maxnamefieldlen then exit;
        if (b = 0) then exit;
        packet.payload[a-b-1] := b;
        b := 0;
      end else begin
        packet.payload[a] := byte(s[a]);
        inc(b);
      end;
    end;
    if b > maxnamefieldlen then exit;
    packet.payload[length(s)-b] := b;
    result := length(s) + 12+5;
  end;

  arr[result-1] := 1;
  arr[result-3] := requesttype and $ff;
  arr[result-4] := requesttype shr 8;
end;

function makereversename(const binip:tbinip):string;
var
  name:string;
  a,b:integer;
begin
  name := '';
  if binip.family = AF_INET then begin
    b := htonl(binip.ip);
    for a := 0 to 3 do begin
      name := name + inttostr(b shr (a shl 3) and $ff)+'.';
    end;
    name := name + 'in-addr.arpa';
  end else
  {$ifdef ipv6}
  if binip.family = AF_INET6 then begin
    for a := 15 downto 0 do begin
      b := binip.ip6.u6_addr8[a];
      name := name + hexchars[b and $f]+'.'+hexchars[b shr 4]+'.';
    end;
    name := name + 'ip6.arpa';
  end else
  {$endif}
  begin
    {empty name}
  end;
  result := name;
end;

{
decodes DNS format name to a string. does not includes the root dot.
doesnt read beyond len.
empty result + non null failurereason: failure
empty result + null failurereason: internal use
}
function decodename(const packet:tdnspacket;len,start,recursion:integer;var numread:integer):string;
var
  arr:array[0..sizeof(packet)-1] of byte absolute packet;
  s:string;
  a,b:integer;
begin
  numread := 0;
  repeat
    if (start+numread < 0) or (start+numread >= len) then begin
      result := '';
      failurereason := 'decoding name: got out of range1';
      exit;
    end;
    b := arr[start+numread];
    if b >= $c0 then begin
      {recursive sub call}
      if recursion > 10 then begin
        result := '';
        failurereason := 'decoding name: max recursion';
        exit;
      end;
      if ((start+numread+1) >= len) then begin
        result := '';
        failurereason := 'decoding name: got out of range3';
        exit;
      end;
      a := ((b shl 8) or arr[start+numread+1]) and $3fff;
      s := decodename(packet,len,a,recursion+1,a);
      if (s = '') and (failurereason <> '') then begin
        result := '';
        exit;
      end;
      if result <> '' then result := result + '.';
      result := result + s;
      inc(numread,2);
      exit;
    end else if b < 64 then begin
      if (numread <> 0) and (b <> 0) then result := result + '.';
      for a := start+numread+1 to start+numread+b do begin
        if (a >= len) then begin
          result := '';
          failurereason := 'decoding name: got out of range2';
          exit;
        end;
        result := result + char(arr[a]);
      end;
      inc(numread,b+1);

      if b = 0 then begin
        if (result = '') and (recursion = 0) then result := '.';
        exit; {reached end of name}
      end;
    end else begin
      failurereason := 'decoding name: read invalid char';
      result := '';
      exit; {invalid}
    end;
  until false;
end;

{==============================================================================}

procedure setstate_return(const rrp:trrpointer;len:integer;var state:tdnsstate);
var
  a:integer;
begin
  state.resultaction := action_done;
  state.resultstr := '';
  case trr(rrp.p^).requesttype of
    querytype_a: begin
      if htons(trr(rrp.p^).datalen) <> 4 then exit;
      move(trr(rrp.p^).data,state.resultbin.ip,4);
      state.resultbin.family :=AF_INET;
    end;
    {$ifdef ipv6}
    querytype_aaaa: begin
      if htons(trr(rrp.p^).datalen) <> 16 then exit;
      state.resultbin.family := AF_INET6;
      move(trr(rrp.p^).data,state.resultbin.ip6,16);
    end;
    {$endif}
  else
    {other reply types (PTR, MX) return a hostname}
    state.resultstr := decodename(state.packet,state.packetlen,integer(rrp.p)-integer(@state.packet)+10,0,a);
  end;
end;

procedure setstate_request_init(const name:string;var state:tdnsstate);
begin
  {destroy things properly}
  state.resultstr := '';
  state.queryname := '';
  state.rrdata := '';
  fillchar(state,sizeof(state),0);
  state.queryname := name;
  state.parsepacket := false;
end;

procedure setstate_forward(const name:string;var state:tdnsstate;family:integer);
begin
  setstate_request_init(name,state);
  state.forwardfamily := family;
  {$ifdef ipv6}
  if family = AF_INET6 then state.requesttype := querytype_aaaa else
  {$endif}
  state.requesttype := querytype_a;
end;

procedure setstate_reverse(const binip:tbinip;var state:tdnsstate);
begin
  setstate_request_init(makereversename(binip),state);
  state.requesttype := querytype_ptr;
end;

procedure setstate_failure(var state:tdnsstate);
begin
  state.resultstr := '';
  fillchar(state.resultbin,sizeof(state.resultbin),0);
  state.resultaction := action_done;
end;

procedure state_process(var state:tdnsstate);
label recursed;
label failure;
var
  a,b,ofs:integer;
  rrtemp:^trr;
  rrptemp:^trrpointer;
begin
  if state.parsepacket then begin
    if state.packetlen < 12 then goto failure;
    if state.id <> state.packet.id then begin
      failurereason := 'ID mismatch';
      state.resultaction := action_ignore;
      exit;
    end;
    state.numrr2 := 0;
    for a := 0 to 3 do begin
      state.numrr1[a] := htons(state.packet.rrcount[a]);
      if state.numrr1[a] > maxrrofakind then goto failure;
      inc(state.numrr2,state.numrr1[a]);
    end;

    setlength(state.rrdata,state.numrr2*sizeof(trrpointer));

    {- put all replies into a list}

    ofs := 12;
    {get all queries}
    for a := 0 to state.numrr1[0]-1 do begin
      if (ofs < 12) or (ofs > state.packetlen-4) then goto failure;
      rrptemp := @state.rrdata[1+a*sizeof(trrpointer)];
      rrptemp.p := @state.packet.payload[ofs-12];
      rrptemp.ofs := ofs;
      decodename(state.packet,state.packetlen,ofs,0,b);
      rrptemp.len := b + 4;
      inc(ofs,rrptemp.len);
    end;

    for a := state.numrr1[0] to state.numrr2-1 do begin
      if (ofs < 12) or (ofs > state.packetlen-12) then goto failure;
      rrptemp := @state.rrdata[1+a*sizeof(trrpointer)];
      if decodename(state.packet,state.packetlen,ofs,0,b) = '' then goto failure;
      rrtemp := @state.packet.payload[ofs-12+b]; {rrtemp points to values and result, after initial name}
      rrptemp.p := rrtemp;
      rrptemp.ofs := ofs; {ofs is start of RR before initial name from start of packet}
      rrptemp.namelen := b;
      b := htons(rrtemp.datalen);
      rrptemp.len := b + 10 + rrptemp.namelen;
      inc(ofs,rrptemp.len);
    end;
    if (ofs <> state.packetlen) then begin
      failurereason := 'ofs <> state.packetlen';
      goto failure;
    end;

    {- check for items of the requested type in answer section, if so return success first}
    for a := state.numrr1[0] to (state.numrr1[0]+state.numrr1[1]-1) do begin
      rrptemp := @state.rrdata[1+a*sizeof(trrpointer)];
      rrtemp := rrptemp.p;
      b := rrptemp.len;
      if rrtemp.requesttype = state.requesttype then begin
        setstate_return(rrptemp^,b,state);
        exit;
      end;
    end;

    {if no items of correct type found, follow first cname in answer section}
    for a := state.numrr1[0] to (state.numrr1[0]+state.numrr1[1]-1) do begin
      rrptemp := @state.rrdata[1+a*sizeof(trrpointer)];
      rrtemp := rrptemp.p;
      b := rrptemp.len;
      if rrtemp.requesttype = querytype_cname then begin
        state.queryname := decodename(state.packet,state.packetlen,rrptemp.ofs+12,0,b);
        goto recursed;
      end;
    end;

    {no cnames found, no items of correct type found}
    if state.forwardfamily <> 0 then goto failure;
{$ifdef ipv6}
    if (state.requesttype = querytype_a) then begin
      {v6 only: in case of forward, look for AAAA in alternative section}
      for a := state.numrr1[0]+state.numrr1[1]+state.numrr1[2] to (state.numrr2-1) do begin
        rrptemp := @state.rrdata[1+a*sizeof(trrpointer)];
        rrtemp := rrptemp.p;
        b := rrptemp.len;
        if rrtemp.requesttype = querytype_aaaa then begin
          setstate_return(rrptemp^,b,state);
          exit;
        end;
      end;
      {no AAAA's found in alternative, do a recursive lookup for them}
      state.requesttype := querytype_aaaa;
      goto recursed;
    end;
{$endif}
    goto failure;
recursed:
    {here it needs recursed lookup}
    {if needing to follow a cname, change state to do so}
    inc(state.recursioncount);
    if state.recursioncount > maxrecursion then goto failure;
  end;

  {here, a name needs to be resolved}
  if state.queryname = '' then begin
    failurereason := 'empty query name';
    goto failure;
  end;

  {do /ets/hosts lookup here}
  state.packetlen := buildrequest(state.queryname,state.packet,state.requesttype);
  if state.packetlen = 0 then begin
    failurereason := 'building request packet failed';
    goto failure;
  end;
  state.id := state.packet.id;
  state.resultaction := action_sendquery;

  exit;
failure:
  setstate_failure(state);
end;
{$ifdef win32}
  const
    MAX_HOSTNAME_LEN = 132;
    MAX_DOMAIN_NAME_LEN = 132;
    MAX_SCOPE_ID_LEN = 260    ;
    MAX_ADAPTER_NAME_LENGTH = 260;
    MAX_ADAPTER_ADDRESS_LENGTH = 8;
    MAX_ADAPTER_DESCRIPTION_LENGTH = 132;
    ERROR_BUFFER_OVERFLOW = 111;
    MIB_IF_TYPE_ETHERNET = 6;
    MIB_IF_TYPE_TOKENRING = 9;
    MIB_IF_TYPE_FDDI = 15;
    MIB_IF_TYPE_PPP = 23;
    MIB_IF_TYPE_LOOPBACK = 24;
    MIB_IF_TYPE_SLIP = 28;


  type
    tip_addr_string=packed record
      Next :pointer;
      IpAddress : array[0..15] of char;
      ipmask    : array[0..15] of char;
      context   : dword;
    end;
    pip_addr_string=^tip_addr_string;
    tFIXED_INFO=packed record
       HostName         : array[0..MAX_HOSTNAME_LEN-1] of char;
       DomainName       : array[0..MAX_DOMAIN_NAME_LEN-1] of char;
       currentdnsserver : pip_addr_string;
       dnsserverlist    : tip_addr_string;
       nodetype         : longint;
       ScopeId          : array[0..MAX_SCOPE_ID_LEN + 4] of char;
       enablerouting    : longbool;
       enableproxy      : longbool;
       enabledns        : longbool;
    end;
    pFIXED_INFO=^tFIXED_INFO;

  var
    iphlpapi : thandle;
    getnetworkparams : function(pFixedInfo : PFIXED_INFO;OutBufLen : plongint) : longint;stdcall;
{$endif}
procedure populatednsserverlist;
var
  {$ifdef win32}
    fixed_info : pfixed_info;
    fixed_info_len : longint;
    currentdnsserver : pip_addr_string;
  {$else}
    t:textfile;
    s:string;
    a:integer;
  {$endif}
begin
  //result := '';
  if assigned(dnsserverlist) then begin
    dnsserverlist.clear;
  end else begin
    dnsserverlist := tstringlist.Create;
  end;
  {$ifdef win32}
    if iphlpapi=0 then iphlpapi := loadlibrary('iphlpapi.dll');
    if not assigned(getnetworkparams) then @getnetworkparams := getprocaddress(iphlpapi,'GetNetworkParams');
    fixed_info_len := 0;
    if GetNetworkParams(nil,@fixed_info_len)<>ERROR_BUFFER_OVERFLOW then exit;
    //fixed_info_len :=sizeof(tfixed_info);
    getmem(fixed_info,fixed_info_len);
    if GetNetworkParams(fixed_info,@fixed_info_len)<>0 then begin
      freemem(fixed_info);
      exit;
    end;
    currentdnsserver := @(fixed_info.dnsserverlist);
    while assigned(currentdnsserver) do begin
      dnsserverlist.Add(currentdnsserver.IpAddress);
      currentdnsserver := currentdnsserver.next;
    end;
    freemem(fixed_info);
  {$else}
    filemode := 0;
    assignfile(t,'/etc/resolv.conf');
    {$i-}reset(t);{$i+}
    if ioresult <> 0 then exit;

    while not eof(t) do begin
      readln(t,s);
      if not (copy(s,1,10) = 'nameserver') then continue;
      s := copy(s,11,500);
      while s <> '' do begin
        if (s[1] = #32) or (s[1] = #9) then s := copy(s,2,500) else break;
      end;
      a := pos(' ',s);
      if a <> 0 then s := copy(s,1,a-1);
      a := pos(#9,s);
      if a <> 0 then s := copy(s,1,a-1);
      //result := s;
      //if result <> '' then break;
      dnsserverlist.Add(s);
    end;
    close(t);
  {$endif}
end;

procedure cleardnsservercache;
begin
  if assigned(dnsserverlist) then begin
    dnsserverlist.destroy;
    dnsserverlist := nil;
  end;
end;

function getcurrentsystemnameserver(var id:integer):string;
var 
  counter : integer;

begin
  if not assigned(dnsserverlist) then populatednsserverlist;
  if dnsserverlist.count=0 then raise exception.create('no dns servers availible');
  id := 0;
  if dnsserverlist.count >1 then begin

    for counter := 1 to dnsserverlist.count-1 do begin
      if longint(dnsserverlist.objects[counter]) < longint(dnsserverlist.objects[id]) then id := counter;
    end;
  end;
  result := dnsserverlist[id]
end;

procedure reportlag(id:integer;lag:integer); //lag should be in microseconds and should be -1 to report a timeout
var
  counter : integer;
  temp : integer;
begin
  if (id < 0) or (id >= dnsserverlist.count) then exit;
  if lag = -1 then lag := timeoutlag;
  for counter := 0 to dnsserverlist.count-1 do begin
    temp := longint(dnsserverlist.objects[counter]) *15;
    if counter=id then temp := temp + lag;
    dnsserverlist.objects[counter] := tobject(temp div 16);
  end;

end;

end.
