// this file contains code copied from linux.pp in the free pascal rtl
// i had to copy them because i use a different definition of fdset to them
// the copyright block from the file in question is shown below
{
   $Id$
   This file is part of the Free Pascal run time library.
   Copyright (c) 1999-2000 by Michael Van Canneyt,
   BSD parts (c) 2000 by Marco van de Voort
   members of the Free Pascal development team.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}
{$mode delphi}
unit fd_utils;
interface

type
    FDSet= Array [0..255] of longint; {31}
    PFDSet= ^FDSet;
const
    absoloutemaxs=(sizeof(fdset)*8)-1;

Procedure FD_Clr(fd:longint;var fds:fdSet);
Procedure FD_Zero(var fds:fdSet);
Procedure FD_Set(fd:longint;var fds:fdSet);
Function FD_IsSet(fd:longint;var fds:fdSet):boolean;


implementation  
uses sysutils;
Procedure FD_Clr(fd:longint;var fds:fdSet);inline;
{ Remove fd from the set of filedescriptors}
begin
  if (fd < 0) then raise exception.create('FD_Clr fd out of range: '+inttostr(fd));
  fds[fd shr 5]:=fds[fd shr 5] and (not (1 shl (fd and 31)));
end;

Procedure FD_Zero(var fds:fdSet);
{ Clear the set of filedescriptors }
begin
  FillChar(fds,sizeof(fdSet),0);
end;

Procedure FD_Set(fd:longint;var fds:fdSet);inline;
{ Add fd to the set of filedescriptors }
begin
  if (fd < 0) then raise exception.create('FD_set fd out of range: '+inttostr(fd));
  fds[fd shr 5]:=fds[fd shr 5] or (1 shl (fd and 31));
end;

Function FD_IsSet(fd:longint;var fds:fdSet):boolean;inline;
{ Test if fd is part of the set of filedescriptors }
begin
  if (fd < 0) then begin
    result := false;
    exit;
  end;
  FD_IsSet:=((fds[fd shr 5] and (1 shl (fd and 31)))<>0);
end;
end.
