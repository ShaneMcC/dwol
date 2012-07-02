{*
 * DebugList - Easy way of storing debug information as a stacktrace
 * Copyright (C) 2005 Shane "Dataforce" Mc Cormack
 * For conditions of distribution and use, see copyright notice in license.txt
 *
 * SVN: $Id:$
 *}
unit DebugList;

{$I defines.inc}

interface

uses
  SysUtils, Classes, svn;

type
  TDebugList = class
    constructor Create(MaxSize: Integer = 15);
    destructor Destroy(); override;
    private
      TheList: TStringList;
      Size: Integer;
    public
      procedure AddItem(ItemInfo: String);
      procedure Reset();
      function Resize(NewSize: Integer): Boolean;
      function GetItem(Index: Integer): String;
      function MaxItem(): Integer;
  end;

implementation

destructor TDebugList.Destroy();
begin
  TheList.Clear;
  TheList.Destroy;
  inherited;
end;


constructor TDebugList.Create(MaxSize: Integer = 15);
begin
  TheList := TStringList.Create;
  Size := MaxSize;
  TheList.Capacity := MaxSize;
end;

procedure TDebugList.AddItem(ItemInfo: String);
begin
  while TheList.Count >= Size do begin
    TheList.Delete(0);
  end;
  TheList.Add(ItemInfo);
end;

function TDebugList.GetItem(Index: Integer): String;
begin
  if Index < TheList.Count then begin
    Result := TheList.Strings[Index];
  end
  else begin
    Result := '';
  end;
end;

function TDebugList.MaxItem(): Integer;
begin
  Result := TheList.Count-1;
end;

procedure TDebugList.Reset();
begin
  TheList.Clear;
end;

function TDebugList.Resize(NewSize: Integer): Boolean;
begin
  Result := true;
  try
    if NewSize = Size then exit;

    if NewSize < Size then begin
      while TheList.Count >= NewSize do begin
        TheList.Delete(0);
      end;
    end;
    
    Size := NewSize;
    TheList.Capacity := NewSize;
  except
    Result := false;
  end;
end;

{---- Adjust global SVN revision ----}
initialization
  SVNRevision('$Id:$');
end.
