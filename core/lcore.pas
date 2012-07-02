{lsocket.pas}

{io and timer code by plugwash}

{ Copyright (C) 2005 Bas Steendijk and Peter Green
  For conditions of distribution and use, see copyright notice in zlib_license.txt
  which is included in the package
  ----------------------------------------------------------------------------- }

{note: you must use the @ in the last param to tltask.create not doing so will
 compile without error but will cause an access violation -pg}

//note: events after release are normal and are the apps responsibility to deal with safely

unit lcore;
{$mode delphi}
interface
  uses
    sysutils,
    {$ifdef VER1_0}
      linux,
    {$else}
      baseunix,unix,
    {$endif}
    classes,pgdebugout,pgtypes,bfifo,fd_utils;

  type
    tsetchangeevent=procedure(fd : integer);
  procedure processtasks;
  procedure rmasterset(fd : integer);
  procedure rmasterclr(fd: integer);
  function  rmasterisset(fd: integer) : boolean;
  
  procedure wmasterset(fd : integer);
  procedure wmasterclr(fd: integer);
  function  wmasterisset(fd: integer) : boolean;
  
  var
    onrmasterset,onrmasterclr,onwmasterset,onwmasterclr:tsetchangeevent;
  
  
  const
    receivebufsize=1460;

  type
    {$ifdef ver1_0}
      sigset= array[0..31] of longint;
    {$endif}

    ESocketException   = class(Exception);
    TBgExceptionEvent  = procedure (Sender : TObject;
                                  E : Exception;
                                  var CanClose : Boolean) of object;
    TSocketState       = (wsInvalidState,
                        wsOpened,     wsBound,
                        wsConnecting, wsConnected,
                        wsAccepting,  wsListening,
                        wsClosed);

    TWSocketOption       = (wsoNoReceiveLoop, wsoTcpNoDelay);
    TWSocketOptions      = set of TWSocketOption;

    // note : tsocketstate is defined in the same way as it is in François PIETTE's twsocket
    // however tlsocket currently only uses wsClosed wsConnecting wsconnected and wsListen3ing
    TSocketevent     = procedure(Sender: TObject; Error: word) of object;
    //Tdataavailevent  = procedure(data : string);
    TSendData          = procedure (Sender: TObject; BytesSent: Integer) of object;

    tlcomponent = class(tcomponent)
    public
      released:boolean;
      procedure release; virtual;
      destructor destroy; override;
    end;

    tlasio = class(tlcomponent)
    public
      state              : tsocketstate      ;
      ComponentOptions   : TWSocketOptions;
      fdhandlein         : Longint           ;  {file discriptor}
      fdhandleout        : Longint           ;  {file discriptor}

      onsessionclosed    : tsocketevent      ;
      ondataAvailable    : tsocketevent      ;
      onsessionAvailable : tsocketevent      ;

      onsessionconnected : tsocketevent      ;
      onsenddata         : tsenddata      ;
      ondatasent         : tsocketevent      ;
      //connected          : boolean         ;
      nextasin           : tlasio            ;
      prevasin           : tlasio            ;

      recvq              : tfifo;
      OnBgException      : TBgExceptionEvent ;
      //connectread        : boolean           ;
      sendq              : tfifo;
      closehandles       : boolean           ;
      writtenthiscycle   : boolean           ;
      lasterror:integer;
      destroying:boolean;
      function receivestr:string; virtual;
      procedure close;
      procedure abort;
      procedure internalclose(error:word); virtual;
      constructor Create(AOwner: TComponent); override;

      destructor destroy; override;
      procedure fdcleanup;
      procedure HandleBackGroundException(E: Exception);
      procedure handlefdtrigger(readtrigger,writetrigger:boolean); virtual;
      procedure dup(invalue:longint);
      function sendflush : integer;
      procedure sendstr(const str : string);virtual;
      procedure putstringinsendbuffer(const newstring : string);
      function send(data:pointer;len:integer):integer;virtual;
      procedure putdatainsendbuffer(data:pointer;len:integer); virtual;
      procedure deletebuffereddata;

      //procedure messageloop;
      function Receive(Buf:Pointer;BufSize:integer):integer; virtual;
      procedure flush;
      procedure dodatasent(wparam,lparam:longint);
      procedure doreceiveloop(wparam,lparam:longint);
      procedure sinkdata(sender:tobject;error:word);

      procedure release; override; {test -beware}
    end;
    ttimerwrapperinterface=class(tlcomponent)
    public
      function createwrappedtimer : tobject;virtual;abstract;
//      procedure setinitialevent(wrappedtimer : tobject;newvalue : boolean);virtual;abstract;
      procedure setontimer(wrappedtimer : tobject;newvalue:tnotifyevent);virtual;abstract;
      procedure setenabled(wrappedtimer : tobject;newvalue : boolean);virtual;abstract;
      procedure setinterval(wrappedtimer : tobject;newvalue : integer);virtual;abstract;
    end;

  var
    timerwrapperinterface : ttimerwrapperinterface;
  type
    tltimer=class(tlcomponent)
    protected

      prevtimer          : tltimer           ;
      nexttimer          : tltimer           ;
      nextts	         : ttimeval          ;
      wrappedtimer : tobject;
      procedure resettimes;

//      finitialevent       : boolean           ;
      fontimer            : tnotifyevent      ;
      fenabled            : boolean           ;
      finterval	          : integer	     ; {miliseconds, default 1000}

//      procedure setinitialevent(newvalue : boolean);
      procedure setontimer(newvalue:tnotifyevent);
      procedure setenabled(newvalue : boolean);
      procedure setinterval(newvalue : integer);
    public


      constructor create(aowner:tcomponent);override;
      destructor destroy;override;
//      property initialevent : boolean read finitialevent write setinitialevent;
      property ontimer : tnotifyevent read fontimer write setontimer;
      property enabled : boolean read fenabled write setenabled;
      property interval	: integer read finterval write setinterval;

    end;

    ttaskevent=procedure(wparam,lparam:longint) of object;

    tltask=class(tobject)
    public
      handler  : ttaskevent;
      obj      : tobject;
      wparam   : longint;
      lparam   : longint;
      nexttask : tltask;
      constructor create(ahandler:ttaskevent;aobj:tobject;awparam,alparam:longint);
    end;

    tlloopback=class(tlasio)
    public
      constructor create(aowner:tcomponent); override;
    end;
var
  firstasin                             : tlasio     ;
  firsttimer                            : tltimer    ;
  firsttask  , lasttask   , currenttask : tltask     ;
  
  numread                               : integer    ;
  maxs                                  : longint    ;
  exitloopflag                          : boolean    ; {if set by app, exit mainloop}
  signalloopback                        : tlloopback ;
  mustrefreshfds                        : boolean    ;
{  lcoretestcount:integer;}
  fdreverse:array[0..absoloutemaxs] of pointer;
  asinreleaseflag:boolean;

procedure messageloop;
procedure disconnecttasks(aobj:tobject);
procedure addtask(ahandler:ttaskevent;aobj:tobject;awparam,alparam:longint);
type
  tonaddtask = procedure(ahandler:ttaskevent;aobj:tobject;awparam,alparam:longint);
var
  onaddtask : tonaddtask;
  
procedure exitmessageloop;
procedure processmessages;

procedure sleep(i:integer);

implementation
uses {sockets,}lsignal;
{$include unixstuff.inc}

var
  fdsrmaster , fdswmaster               : fdset      ;
procedure rmasterset(fd : integer);
begin
  if fd_isset(fd,fdsrmaster) then exit;
  fd_set(fd,fdsrmaster);
  if assigned(onrmasterset) then onrmasterset(fd);
end;

procedure rmasterclr(fd: integer);
begin
  if not fd_isset(fd,fdsrmaster) then exit;
  fd_clr(fd,fdsrmaster);
  if assigned(onrmasterclr) then onrmasterclr(fd);
end;

function  rmasterisset(fd: integer) : boolean;
begin
  result := fd_isset(fd,fdsrmaster);
end;
  
procedure wmasterset(fd : integer);
begin
  if fd_isset(fd,fdswmaster) then exit;
  fd_set(fd,fdswmaster);
  if assigned(onwmasterset) then onwmasterset(fd);
end;

procedure wmasterclr(fd: integer);
begin
  if not fd_isset(fd,fdswmaster) then exit;
  fd_clr(fd,fdswmaster);
  if assigned(onwmasterclr) then onwmasterclr(fd);
end;

function  wmasterisset(fd: integer) : boolean;
begin
  result := fd_isset(fd,fdswmaster);
end;

{!!! added sleep call -beware}
procedure sleep(i:integer);
var
  tv:ttimeval;
begin
  tv.tv_sec := i div 1000;
  tv.tv_usec := (i mod 1000) * 1000;
  select(0,nil,nil,nil,@tv);
end;

destructor tlcomponent.destroy;
begin
  disconnecttasks(self);
  inherited destroy;
end;


{$include ltimevalstuff.inc}

procedure tlcomponent.release;
begin
  released := true;
end;

procedure tlasio.release;
begin
  asinreleaseflag := true;
  inherited release;
end;

procedure tlasio.doreceiveloop;
begin
  if recvq.size = 0 then exit;
  if assigned(ondataavailable) then ondataavailable(self,0);
  if not (wsonoreceiveloop in componentoptions) then
  if recvq.size > 0 then tltask.create(self.doreceiveloop,self,0,0);
end;

function tlasio.receivestr;
begin
  setlength(result,recvq.size);
  receive(@result[1],length(result));
end;

function tlasio.receive(Buf:Pointer;BufSize:integer):integer;
var
  i,a,b:integer;
  p:pointer;
begin
  i := bufsize;
  if recvq.size < i then i := recvq.size;
  a := 0;
  while (a < i) do begin
    b := recvq.get(p,i-a);
    move(p^,buf^,b);
    inc(integer(buf),b);
    recvq.del(b);
    inc(a,b);
  end;
  result := i;
  if wsonoreceiveloop in componentoptions then begin
    if recvq.size = 0 then rmasterset(fdhandlein);
  end;
end;

constructor tlasio.create;
begin
  inherited create(AOwner);
  sendq := tfifo.create;
  recvq := tfifo.create;
  state := wsclosed;
  fdhandlein := -1;
  fdhandleout := -1;
  nextasin := firstasin;
  prevasin := nil;
  if assigned(nextasin) then nextasin.prevasin := self;
  firstasin := self;

  released := false;
end;

destructor tlasio.destroy;
begin
  destroying := true;
  if state <> wsclosed then close;
  if prevasin <> nil then begin
    prevasin.nextasin := nextasin;
  end else begin
    firstasin := nextasin;
  end;
  if nextasin <> nil then begin
    nextasin.prevasin := prevasin;
  end;
  recvq.destroy;
  sendq.destroy;
  inherited destroy;
end;

procedure tlasio.close;
begin
  internalclose(0);
end;

procedure tlasio.abort;
begin
  close;
end;

procedure tlasio.fdcleanup;
begin
  if fdhandlein <> -1 then begin
    rmasterclr(fdhandlein); //fd_clr(fdhandlein,fdsrmaster)
  end;
  if fdhandleout <> -1 then begin
    wmasterclr(fdhandleout);//fd_clr(fdhandleout,fdswmaster)
  end;
  if fdhandlein=fdhandleout then begin
    if fdhandlein <> -1 then begin
      fdclose(fdhandlein);
    end;
  end else begin
    if fdhandlein <> -1 then begin
      fdclose(fdhandlein);
    end;
    if fdhandleout <> -1 then begin
      fdclose(fdhandleout);
    end;
  end;
  fdhandlein := -1;
  fdhandleout := -1;
end;

procedure tlasio.internalclose(error:word);
begin
  if state<>wsclosed then begin
    if (fdhandlein < 0) or (fdhandleout < 0) then raise exception.create('internalclose called with invalid fd handles');
    rmasterclr(fdhandlein);//fd_clr(fdhandlein,fdsrmaster);
    wmasterclr(fdhandleout);//fd_clr(fdhandleout,fdswmaster);

    if closehandles then begin
      fcntl(fdhandlein,F_SETFL,0);
      fdclose(fdhandlein);
      if fdhandleout <> fdhandlein then begin
        fcntl(fdhandleout,F_SETFL,0);
        fdclose(fdhandleout);
      end;
      fdreverse[fdhandlein] := nil;
      fdreverse[fdhandleout] := nil;

      fdhandlein := -1;
      fdhandleout := -1;
    end;
    state := wsclosed;

    if assigned(onsessionclosed) then if not destroying then onsessionclosed(self,error);
  end;
  sendq.del(maxlongint);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ All exceptions *MUST* be handled. If an exception is not handled, the     }
{ application will most likely be shut down !                               }
procedure tlasio.HandleBackGroundException(E: Exception);
var
  CanAbort : Boolean;
begin
  CanAbort := TRUE;
  { First call the error event handler, if any }
  if Assigned(OnBgException) then begin
    try
      OnBgException(Self, E, CanAbort);
    except
    end;
  end;
  { Then abort the socket }
  if CanAbort then begin
    try
      close;
    except
    end;
  end;
end;

procedure tlasio.sendstr(const str : string);
begin
  putstringinsendbuffer(str);
  sendflush;
end;

procedure tlasio.putstringinsendbuffer(const newstring : string);
begin
  if newstring <> '' then putdatainsendbuffer(@newstring[1],length(newstring));
end;

function tlasio.send(data:pointer;len:integer):integer;
begin
  if state <> wsconnected then begin
    result := -1;
    exit;
  end;
  if len < 0 then len := 0;
  result := len;
  putdatainsendbuffer(data,len);
  sendflush;
end;


procedure tlasio.putdatainsendbuffer(data:pointer;len:integer);
begin
  sendq.add(data,len);
end;

function tlasio.sendflush : integer;
var
  lensent : integer;
  data:pointer;
begin
  if state <> wsconnected then exit;

  lensent := sendq.get(data,2920);
  if assigned(data) then result := fdwrite(fdhandleout,data^,lensent) else result := 0;

  if result = -1 then lensent := 0 else lensent := result;

  //sendq := copy(sendq,lensent+1,length(sendq)-lensent);
  sendq.del(lensent);

  //fd_clr(fdhandleout,fdsw); // this prevents the socket being closed by a write
                            // that sends nothing because a previous socket has
                            // slready flushed this socket when the message loop
                            // reaches it
//  if sendq.size > 0 then begin
    wmasterset(fdhandleout);//fd_set(fdhandleout,fdswmaster);
//  end else begin
//    wmasterclr(fdhandleout);//fd_clr(fdhandleout,fdswmaster);
//  end;
  if result > 0 then begin
    if assigned(onsenddata) then onsenddata(self,result);
//    if sendq.size=0 then if assigned(ondatasent) then begin
//      tltask.create(self.dodatasent,self,0,0);
//      //begin test code
//      fd_zero(fdstestr);
//      fd_zero(fdstestw);
//      fd_set(fdhandlein,fdstestr);
//      fd_set(fdhandleout,fdstestw);
//      select(maxs,@fdstestr,@fdstestw,nil,0);
//      writeln(fd_isset(fdhandlein,fdstestr),' ',fd_isset(fdhandleout,fdstestw));
//      //end test code
//    
//    end;
    writtenthiscycle := true;
  end;
end;

procedure tlasio.dup(invalue:longint);
begin
{  debugout('invalue='+inttostr(invalue));}
  //readln;
  if state<> wsclosed then close;
  fdhandlein := invalue;
  fdhandleout := invalue;
  fdreverse[fdhandlein] := self;
  fcntl(fdhandlein,F_SETFL,OPEN_NONBLOCK);
  if fdhandlein > absoloutemaxs then raise esocketexception.create('file discriptor out of range');
  if fdhandlein > maxs then maxs := fdhandlein;
  state := wsconnected;
  rmasterset(fdhandlein);//fd_set(fdhandlein,fdsrmaster);
  wmasterclr(fdhandleout);//fd_clr(fdhandleout,fdswmaster);
end;

procedure tlasio.handlefdtrigger(readtrigger,writetrigger:boolean);
var
  sendflushresult : integer;
  tempbuf:array[0..receivebufsize-1] of byte;
begin
  if (state=wsconnected) and writetrigger then begin
    if (sendq.size >0) then begin

      sendflushresult := sendflush;
      if (sendflushresult <= 0) and (not writtenthiscycle) then begin
        if sendflushresult=0 then begin // linuxerror := 0;
          internalclose(0);
      
        end else begin  
          internalclose(linuxerror);
        end;
      end;

    end else begin
      //everything is sent fire off ondatasent event
      if fdhandleout >= 0 then wmasterclr(fdhandleout);//fd_clr(fdhandleout,fdswmaster);
      if assigned(ondatasent) then tltask.create(self.dodatasent,self,0,0);
    end;
  end;
  writtenthiscycle := false;
  if (state =wsconnected) and readtrigger then begin
    if recvq.size=0 then begin
      numread := fdread(fdhandlein,tempbuf,sizeof(tempbuf));
      if (numread=0) and (not mustrefreshfds) then begin
        {if i remember correctly numread=0 is caused by eof
        if this isn't dealt with then you get a cpu eating infinite loop
        however if onsessionconencted has called processmessages that could
        cause us to drop to here with an empty recvq and nothing left to read
        and we don't want that to cause the socket to close}

        internalclose(0);
      end else if (numread=-1) then begin
        numread := 0;
        internalclose(linuxerror);
      end else if numread > 0 then recvq.add(@tempbuf,numread);
    end;

    if recvq.size > 0 then begin
      if wsonoreceiveloop in componentoptions then rmasterclr(fdhandlein); //fd_clr(fdhandlein,fdsrmaster);
      if assigned(ondataavailable) then ondataAvailable(self,0);
      if not (wsonoreceiveloop in componentoptions) then if recvq.size > 0 then
      tltask.create(self.doreceiveloop,self,0,0);
    end;
    //until (numread = 0) or (currentsocket.state<>wsconnected);
{    debugout('inner loop complete');}
  end;
end;


procedure tlasio.flush;
var
  fds : fdset;
begin
  fd_zero(fds);
  fd_set(fdhandleout,fds);
  while sendq.size>0 do begin
    select(maxs+1,nil,@fds,nil,nil);
    if sendflush <= 0 then exit;
  end;
end;

procedure tlasio.dodatasent(wparam,lparam:longint);
begin
  if assigned(ondatasent) then ondatasent(self,lparam);
end;

procedure tlasio.deletebuffereddata;
begin
  sendq.del(maxlongint);
end;

procedure tlasio.sinkdata(sender:tobject;error:word);
begin
  tlasio(sender).recvq.del(maxlongint);
end;

procedure tltimer.resettimes;
begin
  gettimeofday(nextts);
  {if not initialevent then} tv_add(nextts,interval);
end;


{procedure tltimer.setinitialevent(newvalue : boolean);
begin
  if newvalue <> finitialevent then begin
    finitialevent := newvalue;
    if assigned(timerwrapperinterface) then begin
      timerwrapperinterface.setinitialevent(wrappedtimer,newvalue);
    end else begin
      resettimes;
    end;
  end;
end;}

procedure tltimer.setontimer(newvalue:tnotifyevent);
begin
  if @newvalue <> @fontimer then begin
    fontimer := newvalue;
    if assigned(timerwrapperinterface) then begin
      timerwrapperinterface.setontimer(wrappedtimer,newvalue);
    end else begin

    end;
  end;

end;


procedure tltimer.setenabled(newvalue : boolean);
begin
  if newvalue <> fenabled then begin
    fenabled := newvalue;
    if assigned(timerwrapperinterface) then begin
      timerwrapperinterface.setenabled(wrappedtimer,newvalue);
    end else begin
      resettimes;
    end;
  end;
end;

procedure tltimer.setinterval(newvalue:integer);
begin
  if newvalue <> finterval then begin
    finterval := newvalue;
    if assigned(timerwrapperinterface) then begin
      timerwrapperinterface.setinterval(wrappedtimer,newvalue);
    end else begin
      resettimes;
    end;
  end;

end;




constructor tltimer.create;
begin
  inherited create(AOwner);
  if assigned(timerwrapperinterface) then begin
    wrappedtimer := timerwrapperinterface.createwrappedtimer;
  end else begin


    nexttimer := firsttimer;
    prevtimer := nil;

    if assigned(nexttimer) then nexttimer.prevtimer := self;
    firsttimer := self;
  end;
  interval := 1000;
  enabled := true;
  released := false;

end;

destructor tltimer.destroy;
begin
  if assigned(timerwrapperinterface) then begin
    wrappedtimer.free;
  end else begin
    if prevtimer <> nil then begin
      prevtimer.nexttimer := nexttimer;
    end else begin
      firsttimer := nexttimer;
    end;
    if nexttimer <> nil then begin
      nexttimer.prevtimer := prevtimer;
    end;
    
  end;
  inherited destroy;
end;

constructor tltask.create(ahandler:ttaskevent;aobj:tobject;awparam,alparam:longint);
begin
  inherited create;
  handler   := ahandler;
  obj       := aobj;
  wparam    := awparam;
  lparam    := alparam;
  {nexttask  := firsttask;
  firsttask := self;}
  if assigned(lasttask) then begin
    lasttask.nexttask := self;
  end else begin
    firsttask := self;
  end;
  lasttask := self;
  //ahandler(wparam,lparam);
end;

procedure addtask(ahandler:ttaskevent;aobj:tobject;awparam,alparam:longint);
begin
  if assigned(onaddtask) then onaddtask(ahandler,aobj,awparam,alparam);
  tltask.create(ahandler,aobj,awparam,alparam);
end;

constructor tlloopback.create(aowner:tcomponent);
begin
  inherited create(aowner);
  closehandles := true;
  assignpipe(fdhandlein,fdhandleout);

  rmasterset(fdhandlein);//fd_set(fdhandlein,fdsrmaster);
  wmasterclr(fdhandlein);//fd_clr(fdhandleout,fdswmaster);
  fdreverse[fdhandlein] := self;
  fdreverse[fdhandleout] := self;
  state := wsconnected;
end;



procedure prepsigpipe;inline;
begin
  starthandlesignal(sigpipe);
  if not assigned(signalloopback) then begin
    signalloopback := tlloopback.create(nil);
    signalloopback.ondataAvailable := signalloopback.sinkdata;

  end;

end;

procedure processtasks;//inline;
var
  temptask                : tltask   ;

begin

  if not assigned(currenttask) then begin
    currenttask := firsttask;
    firsttask := nil;
    lasttask  := nil;
  end;
  while assigned(currenttask) do begin

    if assigned(currenttask.handler) then currenttask.handler(currenttask.wparam,currenttask.lparam);
    if assigned(currenttask) then begin
      temptask := currenttask;
      currenttask := currenttask.nexttask;
      temptask.free;
    end;
  end;
end;

procedure processtimers;inline;
var
  tvnow                   : ttimeval ;
  currenttimer            : tltimer  ;
  temptimer               : tltimer  ;

begin
  gettimeofday(tvnow);
  currenttimer := firsttimer;
  while assigned(currenttimer) do begin
    //writeln(currenttimer.enabled);
    if tv_compare(tvnow,ttimeval(currenttimer.nextts)) and currenttimer.enabled then begin
      //if assigned(currenttimer.ontimer) then begin
      //  if currenttimer.enabled then if currenttimer.initialevent or currenttimer.initialdone then currenttimer.ontimer(currenttimer);
      //  currenttimer.initialdone := true;
      //end;
      if assigned(currenttimer.ontimer) then currenttimer.ontimer(currenttimer);
      currenttimer.nextts := timeval(tvnow);
      tv_add(ttimeval(currenttimer.nextts),currenttimer.interval);
    end;
    temptimer := currenttimer;
    currenttimer := currenttimer.nexttimer;
    if temptimer.released then temptimer.free;
  end;
end;

procedure processasios(var fdsr,fdsw:fdset);//inline;
var
  currentsocket : tlasio  ;
  tempsocket    : tlasio  ;
  socketcount   : integer ; // for debugging perposes :)
  dw,bt:integer;
begin
{  inc(lcoretestcount);}

    //the message loop will exit if all lasio's and ltimer's and lsignal's are destroyed
    //if (not assigned(firstasin)) and (not assigned(firsttimer)) and (not assigned(firstsignal)) then exit;


  {------- test optimised loop}
  socketcount := 0;
  for dw := (maxs shr 5) downto 0 do if (fdsr[dw] or fdsw[dw]) <> 0 then begin
    for bt := 0 to 31 do if (fdsr[dw] or fdsw[dw]) and (1 shl bt) <> 0 then begin
      inc(socketcount);
      currentsocket := fdreverse[dw shl 5 or bt];
      {if not assigned(currentsocket) then raise exception.create('currentsocket not assigned');
      if currentsocket.fdhandlein < 0 then raise exception.create('currentsocket.fdhandlein out of range');}
      {i've seen the out of range case actually happening, so it can happen. test: just close the fd - beware}
      if not assigned(currentsocket) then begin
        fdclose(dw shl 5 or bt);
        continue
      end;
      if currentsocket.fdhandlein < 0 then begin
        fdclose(dw shl 5 or bt);
        continue
      end;
      try
        currentsocket.handlefdtrigger(fd_isset(currentsocket.fdhandlein,fdsr),fd_isset(currentsocket.fdhandleout,fdsw));
      except
        on E: exception do begin
          currentsocket.HandleBackGroundException(e);
        end;
      end;

      if mustrefreshfds then begin
        if select(maxs+1,@fdsr,@fdsw,nil,0) <= 0 then begin
          fd_zero(fdsr);
          fd_zero(fdsw);
        end;
      end;
    end;
  end;

  if asinreleaseflag then begin
    asinreleaseflag := false;
    currentsocket := firstasin;
    while assigned(currentsocket) do begin
      tempsocket := currentsocket;
      currentsocket := currentsocket.nextasin;
      if tempsocket.released then begin
        tempsocket.free;
      end;
    end;
  end;
  {
  !!! issues:
  - sockets which are released may not be freed because theyre never processed by the loop
  made new code for handling this, using asinreleaseflag

  - when/why does the mustrefreshfds select apply, sheck if i did it correctly?

  - what happens if calling handlefdtrigger for a socket which does not have an event
  }
  {------- original loop}

  (*
  currentsocket := firstasin;
  socketcount := 0;
  while assigned(currentsocket) do begin
    if mustrefreshfds then begin
      if select(maxs,@fdsr,@fdsw,nil,0) <= 0 then begin
        fd_zero(fdsr);
        fd_zero(fdsw);
      end;
    end;
    try
      if fd_isset(currentsocket.fdhandlein,fdsr) or fd_isset(currentsocket.fdhandleout,fdsw) then begin
        currentsocket.handlefdtrigger(fd_isset(currentsocket.fdhandlein,fdsr),fd_isset(currentsocket.fdhandleout,fdsw));
      end;
    except
      on E: exception do begin
        currentsocket.HandleBackGroundException(e);
      end;
    end;
    tempsocket := currentsocket;
    currentsocket := currentsocket.nextasin;
    inc(socketcount);
    if tempsocket.released then begin
      tempsocket.free;
    end;
  end; *)
{  debugout('socketcount='+inttostr(socketcount));}
end;

procedure processmessages;
var
  fdsr         , fdsw : fdset   ;
  selectresult        : longint ;
begin
  mustrefreshfds := false;
  prepsigpipe;
  selectresult := select(maxs+1,@fdsr,@fdsw,nil,0);
  while (selectresult>0) or assigned(firsttask) or assigned(currenttask) do begin;

    processtasks;
    processtimers;
    if selectresult > 0 then begin
      processasios(fdsr,fdsw);
    end;
    selectresult := select(maxs+1,@fdsr,@fdsw,nil,0);

  end;
  mustrefreshfds := true;
end;


var
  FDSR , FDSW : fdset;

Function  doSelect(timeOut:PTimeVal):longint;//inline;
var
  localtimeval : ttimeval;
  maxslocal    : integer;
begin
  //unblock signals
  //zeromemory(@sset,sizeof(sset));
  //sset[0] := ;
  fdsr := fdsrmaster;
  fdsw := fdswmaster;

  if assigned(firsttask) then begin
    localtimeval.tv_sec  := 0;
    localtimeval.tv_usec := 0;
    timeout := @localtimeval;
  end;

  maxslocal := maxs;
  mustrefreshfds := false;
{  debugout('about to call select');}
  sigprocmask(SIG_UNBLOCK,@blockset,nil);

  result := select(maxslocal+1,@FDSR,@FDSW,nil,timeout);
  if result <= 0 then begin
    fd_zero(FDSR);
    fd_zero(FDSW);
    if result=-1 then begin
      if linuxerror = SYS_EINTR then begin
        // we received a signal it's not a problem
      end else begin
        raise esocketexception.create('select returned error '+inttostr(linuxerror));
      end;
    end;
  end;
  sigprocmask(SIG_BLOCK,@blockset,nil);
{  debugout('select complete');}
end;

procedure exitmessageloop;
begin
  exitloopflag := true
end;

procedure messageloop;
var
  tv           ,tvnow     : ttimeval ;
  currenttimer            : tltimer  ;
  selectresult:integer;
begin
  prepsigpipe;
  {currentsocket := firstasin;
  if not assigned(currentsocket) then exit; //the message loop will exit if all lsockets are destroyed
  repeat

    if currentsocket.state = wsconnected then currentsocket.sendflush;
    currentsocket := currentsocket.nextasin;
  until not assigned(currentsocket);}


  repeat

    //the message loop will exit if all lasio's and ltimer's and lsignal's are destroyed
    if (not assigned(firstasin)) and (not assigned(firsttimer)) and (not assigned(firstsignal)) then exit;
    {fd_zero(FDSR);
    fd_zero(FDSW);
    currentsocket := firstasin;
    if not assigned(currentsocket) then exit; //the message loop will exit if all lsockets are destroyed
    
    repeat
      if (not currentsocket.released) and (currentsocket.state<>wsclosed) then fd_set(currentsocket.fdhandlein,fdsr);
      if (not currentsocket.released) and (currentsocket.state=wsconnecting) then fd_set(currentsocket.fdhandleout,fdsw);
      if currentsocket is tlsocket then begin
         if (not currentsocket.released) and (currentsocket.state=wsconnected) and(tlsocket(currentsocket).sendq <> '') then fd_set(currentsocket.fdhandleout,fdsw);
      end;
      tempsocket := currentsocket;
      currentsocket := currentsocket.nextasin;
      if tempsocket.released then begin
        tempsocket.free;
      end;
    until not assigned(currentsocket);
    }
    processtasks;
    //currenttask := nil;
    {beware}
    //if assigned(firsttimer) then begin
    //  tv.tv_sec := maxlongint;
    tv := tv_invalidtimebig;
    currenttimer := firsttimer;
    while assigned(currenttimer) do begin
      if tv_compare(tv,currenttimer.nextts) and currenttimer.enabled then tv := currenttimer.nextts;
      currenttimer := currenttimer.nexttimer;
    end;


    if tv_compare(tv,tv_invalidtimebig) then begin    
      //writeln('no timers active');
      if exitloopflag then break;
{    sleep(10);}
      selectresult := doselect(nil);
      
    end else begin
      gettimeofday(tvnow);
      tv_substract(tv,tvnow);
    
      //writeln('timers active');
      if tv.tv_sec < 0 then begin
        tv.tv_sec := 0;
        tv.tv_usec := 0; {0.1 sec}
      end;
      if exitloopflag then break;
{    sleep(10);}
      selectresult := doselect(@tv);
      processtimers;
     
    end;
    if selectresult > 0 then processasios(fdsr,fdsw);
    {!!!only call processasios if select has asio events -beware}

    {artificial delay to throttle the number of processasios per second possible and reduce cpu usage}
  until false;
end;

procedure disconnecttasks(aobj:tobject);
var
  currenttasklocal : tltask ;
  counter          : byte   ;
begin
  for counter := 0 to 1 do begin
    if counter = 0 then begin
      currenttasklocal := firsttask; //main list of tasks
    end else begin
      currenttasklocal := currenttask; //needed in case called from a task
    end;
    // note i don't bother to sestroy the links here as that will happen when
    // the list of tasks is processed anyway
    while assigned(currenttasklocal) do begin
      if currenttasklocal.obj = aobj then begin
        currenttasklocal.obj := nil;
        currenttasklocal.handler := nil;
      end;
      currenttasklocal := currenttasklocal.nexttask;
    end;
  end;
end;

begin
  firstasin := nil;
  firsttask := nil;
  maxs := 0;
  fd_zero(fdsrmaster);
  fd_zero(fdswmaster);
  signalloopback := nil;
end.





