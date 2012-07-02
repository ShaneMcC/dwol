// -----------------------------------------------------------------------------
// IP <- -> String
// (c) Michael Nixon 2005.
// -----------------------------------------------------------------------------
unit ipstrings;

interface

uses sysutils, classes;

function zip_ipstrtoint(s: string): longint;
function ipstrtoint(s: string): longint;
function ipinttostr(i: longint): string;

implementation
uses common;

function ipstrtoint(s: string): longint;
var
  Words: TStringList;
  IP: array [0..3] of byte;
  i: Integer;
begin
  Words := nil;
  Split('.',s,Words);
  for I := 0 to 3 do IP[I] := strtoint(Words[I]);
  Result := (IP[0] shl 24) or (IP[1] shl 16) or (IP[2] shl 8) or (IP[3]);
  Words.Clear;
  Words.Free;
end;

function ipinttostr(i: longint): string;
begin
  result := inttostr((i shr 24) and $FF) + '.' + inttostr((i shr 16) and $FF) + '.' +
            inttostr((i shr 8) and $FF) + '.' + inttostr(i and $FF);
end;

function zip_ipstrtoint(s: string): longint;
var
  a: string;
  i: longint;
  x: longint;
begin
  result := 0;

  a := s;

  x := pos('.', a);
  i := strtoint(copy(a, 1, x - 1));
  a := copy(a, x + 1, length(a) - x);
  result := result or i;

  x := pos('.', a);
  i := strtoint(copy(a, 1, x - 1));
  a := copy(a, x + 1, length(a) - x);
  result := result or (i shl 8);

  x := pos('.', a);
  i := strtoint(copy(a, 1, x - 1));
  a := copy(a, x + 1, length(a) - x);
  result := result or (i shl 16);

  i := strtoint(a);
  result := result or (i shl 24);
end;

function zip_ipinttostr(i: longint): string;
begin
  result := inttostr(i and $FF) + '.' + inttostr((i shr 8) and $FF) + '.' +
            inttostr((i shr 16) and $FF) + '.' + inttostr((i shr 24) and $FF);
end;

end.
