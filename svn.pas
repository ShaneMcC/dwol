{*
 * svn - Store global SVN Revision information. 
 * Copyright (C) 2005 Shane "Dataforce" Mc Cormack
 * For conditions of distribution and use, see copyright notice in license.txt
 *
 * Based on http://hinata.zipplet.co.uk/wiki/index.php?svntracking2
 * Copywrite (C) 2005 Michael "Zipplet" Nixon
 *
 * SVN: $Id$
 *}
unit svn;

Interface

uses sysutils, classes;

var
  SVN_REVISION: Integer;
  SVN_DETAILS: TStringList;
  SVN_ISCREATED: Boolean;

procedure SVNRevision(IDString: string);

Implementation
uses common;

procedure SVNRevision(IDString: string);
var
  List: TStringList;
  I: Integer;
begin
  if not SVN_ISCREATED then begin
    SVN_REVISION := 0;
    SVN_DETAILS := TStringList.create;
    SVN_ISCREATED := True;
    SVNRevision('$Id$');
  end;

  SVN_DETAILS.Add(IDString);
  List := Nil;
  Split(' ',IDString,List);
  if List.count > 3 then begin
    if IsInt(List[2]) then begin
      I := strtoint(List[2]);
      if I > SVN_REVISION then SVN_REVISION := I;
    end;
  end;
  List.Clear;
  List.Free;
end;

Initialization
  if not SVN_ISCREATED then begin
    SVN_REVISION := 0;
    SVN_DETAILS := TStringList.create;
    SVN_ISCREATED := True;
  end;
  SVNRevision('$Id$');
Finalization
  SVN_DETAILS.destroy;
end.