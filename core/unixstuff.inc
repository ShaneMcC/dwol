{$ifdef UNIX}
  {$macro on}
  {$ifdef VER1_0}
    {$define tv_sec := sec}
    {$define tv_usec := usec}
    function dup(const original:integer):integer;inline;
    begin
      linux.dup(original,result);
    end;
    {$define gettimeofdaysec := gettimeofday}
  {$else}
    
    {$define sigprocmask := fpsigprocmask}
    {$define sigaction   := fpsigaction}
    {$define fdclose     := fpclose}
    {$define fcntl       := fpfcntl}
    {$define fdwrite     := fpwrite}
    {$define fdread      := fpread}
    {$define fdopen      := fpopen}
    {$define select      := fpselect}
    {$define linuxerror  := fpgeterrno}
    {$define fork        := fpfork}
    {$define getpid      := fpgetpid}
    {$define getenv      := fpgetenv}
    {$define chmod       := fpchmod}
    {$define dup2        := fpdup2}
    {$ifndef ver1_9_2}
      {$define flock       := fpflock}
    {$endif}
    procedure gettimeofday(var tv:ttimeval);inline;
    begin
      fpgettimeofday(@tv,nil);    
    end;
    function gettimeofdaysec : longint;
    var
      tv:ttimeval;
    begin
      gettimeofday(tv);
      result := tv.tv_sec;
    end;

    //a function is used here rather than a define to prevent issues with tlasio.dup
    function dup(const original:integer):integer;inline;
    begin
      result := fpdup(original);
    end;
    function octal(invalue:longint):longint;
    var
      a : integer;
      i : integer;
    begin
      i := 0;
      result := 0;
      while invalue <> 0 do begin
        a := invalue mod 10;
        result := result + (a shl (i*3));

        invalue := invalue div 10;
        inc(i);
      end;
    end;
    const
      sys_eintr=esyseintr;

  {$endif}
{$endif}
