{lsocket.pas}

{signal code by plugwash}

{ Copyright (C) 2005 Bas Steendijk and Peter Green
  For conditions of distribution and use, see copyright notice in license.txt
  which is included in the package
  ----------------------------------------------------------------------------- }
      
unit lsignal;
{$mode delphi}
interface
  uses sysutils,
    {$ifdef VER1_0}
      linux,
    {$else}
      baseunix,unix,
    {$endif}
    classes,lcore;

  type
    tsignalevent=procedure(sender:tobject;signal:integer) of object;
    tlsignal=class(tcomponent)
    public
      onsignal           : tsignalevent      ;
      prevsignal         : tlsignal          ;
      nextsignal         : tlsignal          ;

      constructor create(aowner:tcomponent);override;
      destructor destroy;override;
    end;

  const
    {$ifdef VER1_0}
      blockset : sigset=($FFFFFFFF - (1 shr sigstop) - (1 shr sigkill),0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
    {$else}
      {$ifdef darwin}
        blockset : tsigset=(longint($FFFFFFFF - (1 shr sigstop) - (1 shr sigkill)));
      {$else}
        blockset : tsigset=(longint($FFFFFFFF - (1 shr sigstop) - (1 shr sigkill)),0,0,0);
      {$endif}
    {$endif}
  procedure starthandlesignal(signal:integer);

var
  firstsignal : tlsignal;

implementation
{$include unixstuff.inc}

constructor tlsignal.create;
begin
  inherited create(AOwner);
  nextsignal := firstsignal;
  prevsignal := nil;

  if assigned(nextsignal) then nextsignal.prevsignal := self;
  firstsignal := self;

  //interval := 1000;
  //enabled := true;
  //released := false;
end;

destructor tlsignal.destroy;
begin
  if prevsignal <> nil then begin
    prevsignal.nextsignal := nextsignal;
  end else begin
    firstsignal := nextsignal;
  end;
  if nextsignal <> nil then begin
    nextsignal.prevsignal := prevsignal;
  end;
  inherited destroy;
end;
{$ifdef linux}
  {$ifdef ver1_9_8}
    {$define needsignalworkaround}
  {$endif}
  {$ifdef ver2_0_0}
    {$define needsignalworkaround}
  {$endif}
{$endif}
{$ifdef needsignalworkaround}
  //using the 1.9.6 version of this stuff because the 1.9.8 and 2.0.0 versions seem broken
  type
    TSysParam  = Longint;
    TSysResult = longint;
  const
            syscall_nr_sigaction		= 67;
  //function Do_SysCall(sysnr:TSysParam):TSysResult;  {$ifndef VER1_0} oldfpccall; {$endif} external name 'FPC_SYSCALL0';
  //function Do_SysCall(sysnr,param1:TSysParam):TSysResult; {$ifndef VER1_0} oldfpccall; {$endif} external name 'FPC_SYSCALL1';
  //function Do_SysCall(sysnr,param1,param2:TSysParam):TSysResult;  {$ifndef VER1_0} oldfpccall; {$endif} external name 'FPC_SYSCALL2';
  function Do_SysCall(sysnr,param1,param2,param3:TSysParam):TSysResult; {$ifndef VER1_0} oldfpccall; {$endif} external name 'FPC_SYSCALL3';
  //function Do_SysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult; {$ifndef VER1_0} oldfpccall; {$endif} external name 'FPC_SYSCALL4';
  //function Do_SysCall(sysnr,param1,param2,param3,param4,param5:TSysParam):TSysResult;  {$ifndef VER1_0} oldfpccall; {$endif} external name 'FPC_SYSCALL5';

  function Fpsigaction(sig: cint; act : psigactionrec; oact : psigactionrec): cint;// [public, alias : 'FPC_SYSC_SIGACTION'];
  {
    Change action of process upon receipt of a signal.
    Signum specifies the signal (all except SigKill and SigStop).
    If Act is non-nil, it is used to specify the new action.
    If OldAct is non-nil the previous action is saved there.
  }
  begin
  //writeln('fucking');
  {$ifdef RTSIGACTION}
    {$ifdef cpusparc}
      { Sparc has an extra stub parameter }
      Fpsigaction:=do_syscall(syscall_nr_rt_sigaction,TSysParam(sig),TSysParam(act),TSysParam(oact),TSysParam(PtrInt(@Fprt_sigreturn_stub)-8),TSysParam(8));
    {$else cpusparc}
      Fpsigaction:=do_syscall(syscall_nr_rt_sigaction,TSysParam(sig),TSysParam(act),TSysParam(oact),TSysParam(8));
    {$endif cpusparc}
  {$else RTSIGACTION}
    //writeln('nice');
    Fpsigaction:=do_syscall(syscall_nr_sigaction,TSysParam(sig),TSysParam(act),TSysParam(oact));
  {$endif RTSIGACTION}
  end;
{$endif}

// cdecl procedures are not name mangled
// so USING something unlikely to cause colliesions in the global namespace
// is a good idea
procedure lsignal_handler( Sig : Integer);cdecl;//{$ifndef VER1_0}{$ifndef VER1_9_4}{$ifndef VER1_9_6};fucking:psiginfo;compiler:psigcontext{$endif}{$endif}{$endif});cdecl;
var
  currentsignal : tlsignal;
begin
//  writeln('in lsignal_hanler');
  currentsignal := firstsignal;
  while assigned(currentsignal) do begin
    if assigned(currentsignal.onsignal) then currentsignal.onsignal(currentsignal,sig);
    currentsignal := currentsignal.nextsignal;

  end;
//  writeln('about to send down signalloopback');
  if assigned(signalloopback) then begin
    signalloopback.sendstr(' ');
  end;
//  writeln('left lsignal_hanler');
end;

const
  {$ifdef ver1_0}
    saction : sigactionrec = (handler:(sh:lsignal_handler);sa_mask:$FFFFFFFF - (1 shr sigstop) - (1 shr sigkill);sa_flags:0);
  {$else}
    {$ifdef darwin}
      saction : sigactionrec = (sa_handler:tsigaction(lsignal_handler);sa_mask:(longint($FFFFFFFF - (1 shr sigstop) - (1 shr sigkill)));sa_flags:0);
    {$else}
      {$ifdef freebsd}
        saction : sigactionrec = (sa_handler:tsigaction(lsignal_handler);sa_flags:0;sa_mask:(longint($FFFFFFFF - (1 shr sigstop) - (1 shr sigkill)),0,0,0));
      {$else}
        {$ifdef ver1_9_2}
          saction : sigactionrec = (handler:(sh:lsignal_handler);sa_mask:(longint($FFFFFFFF - (1 shr sigstop) - (1 shr sigkill)),0,0,0);sa_flags:0);
        {$else}

          saction : sigactionrec = (sa_handler:{$ifndef ver1_9_6}{$ifndef ver1_9_6}{$ifndef ver1_0}SigActionHandler{$endif}{$endif}{$endif}(lsignal_handler);sa_mask:(longint($FFFFFFFF - (1 shr sigstop) - (1 shr sigkill)),0,0,0);sa_flags:0);
        {$endif}
      {$endif}
    {$endif}
  {$endif}
procedure starthandlesignal(signal:integer);

begin
  if signal in ([0..31]-[sigkill,sigstop]) then begin
    sigprocmask(SIG_BLOCK,@blockset,nil);
    sigaction(signal,@saction,nil)
  end else begin
    raise exception.create('invalid signal number')
  end;
end;


end.
