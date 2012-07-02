{pgdebugout.pas}

{debug output code originally for for bserv}

{ Copyright (C) 2005 Bas Steendijk and Peter Green
  For conditions of distribution and use, see copyright notice in zlib_license.txt
  which is included in the package
  ----------------------------------------------------------------------------- }

unit pgdebugout;
{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses sysutils;
procedure debugout(s:string);

const
  // debug mode bitflags
  DMNone=0;
  DMConsole=1;
  DMFile=2 ;

var
  debugmode : byte;
  debugfile : string;
implementation

procedure debugout(s:string);
var
  t : text;
begin
  if (debugmode and DMConsole)<>0 then begin
    {$ifdef win32}
      if isconsole then writeln(s);
    {$else}
      writeln(s);
    {$endif}
  end;
  if (debugmode and DMFile)<>0 then begin
    assign(t,debugfile);
    if fileexists(debugfile) then begin
      append(t);
    end else begin
      rewrite(t);
    end;

    writeln(t,s);
    closefile(t);
  end;
end;

end.
