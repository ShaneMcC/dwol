(*
 * Simple CLI Param Parser.
 * Copyright (C) 2006 - Shane "Dataforce" Mc Cormack
 * Licensed under the Zlib license. Details of which can be found: http://home.dataforce.org.uk/?p=license&l=zlib
 *
 * Takes the values from the commandline and separates into parameters.
 * Paramenters can be casesensitive or non-case sensitive (Set by constructor, defaults to non-casesensitive)
 *
 * Parameters can be passed in 3 ways:
 *   /param
 *   -param
 *   --param
 *
 * -- on its own will terminate parameter parsing.
 * Anything before the first parameter will be ignored.
 *
 * A \ at the beginning of a word will be dropped (ie --foo \bar gives foo the value of 'bar')
 * this is to allow \ / or - as part of a param (eg --foo bar \-baz gives foo the value 'bar -baz')
 *
 * A parameter can be issued multiple times!
 * When a parameter is given multiple times, the First name will be used (if case insensitive)
 * and the LAST given value will be used.
 *
 * Example:
 * --Foo Bar --FOO baz
 *
 * sName: Foo | sValue: baz | nCount: 2
 *
 * --Foo Bar --FOO baz -foo
 *
 * sName: Foo | sValue: baz | nCount: 3
 *
 * Parsing is done on Create using the arguments passed to the program. (Desired effect in most cases)
 * To Parse an alternative set of arguments, call ParseArgs('<Params>'); (You may wish to call Empty(); First);
 *)
unit CLIParser;

interface

uses Classes, SysUtils;

type
  tParam = record
    sName: String;      // Name of parameter (When multiple poarams are given
                        // with the same name, the FIRST one is used)
    sValue: String;     // Value of parameter (When multiple poarams are given
                        // with the same name, the LAST one is used)
    nCount: Integer;    // How many times has a parameter been given?
  end;
  ptParam = ^tParam;

  TCLIParser = class
    constructor Create(bCaseSensitive: Boolean = false);
    destructor Destroy(); override;
    private
      bIsCaseSensitive: Boolean;
      lParameters: TList;

      procedure NewParam(var Param: ptParam);
      procedure RemoveParam(var Param: ptParam);
      procedure Split(Deliminator: Char; Target: String; var Res: TStringList);
    public
      function ParseArgs(sParamString: String = ''): Integer;    
      function Count(): Integer;
      function Get(nIndex: Integer): ptParam; overload;
      function Exists(sName: String): Integer;
      procedure Empty();      
  end;

implementation
  //----------------------------------------------------------------------------
  // PRIVATE
  //----------------------------------------------------------------------------
  procedure TCLIParser.NewParam(var Param: ptParam);
  begin
    new(Param);
    Param.sName := '';
    Param.sValue := '';
    Param.nCount := 0;
  end;

  procedure TCLIParser.RemoveParam(var Param: ptParam);
  begin
    Param.sName := '';
    Param.sValue := '';
    Param.nCount := 0;
    Dispose(Param);
  end;

  procedure TCLIParser.Split(Deliminator: Char; Target: String; var Res: TStringList);
  var
    i: Integer;
    t: String;
  begin
    if Res <> nil then begin
      Res.free;
      Res := nil;
    end;
    Res := TStringList.Create;
    t := '';
    for i := 1 to Length(Target) do begin
      if Copy(Target,i,1) = Deliminator then begin
        Res.Add(t);
        t := '';
      end else t := t + Copy(Target,i,1);
    end;
    if t <> '' then Res.Add(t);
  end;

  //----------------------------------------------------------------------------
  // Public
  //----------------------------------------------------------------------------
  constructor TCLIParser.Create(bCaseSensitive: Boolean = false);
  begin
    lParameters := TList.Create;
    bIsCaseSensitive := bCaseSensitive;

    Self.ParseArgs;
  end;

  destructor TCLIParser.Destroy;
  begin
    Self.Empty;
    lParameters.Destroy;
    inherited;
  end;

  procedure TCLIParser.Empty();
  var
    I: Integer;
    pTemp: ptParam;  
  begin
    for I := 0 to lParameters.Count-1 do begin
      pTemp := ptParam(lParameters[I]);
      Self.RemoveParam(pTemp);
    end;
    lParameters.Clear;
  end;

  function TCLIParser.Count(): Integer;
  begin
    Result := lParameters.Count;
  end;

  function TCLIParser.Get(nIndex: Integer): ptParam;
  begin
    if (nIndex < lParameters.Count) then Result := ptParam(lParameters[nIndex])
    else Result := nil;
  end;

  function TCLIParser.Exists(sName: String): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := 0 to lParameters.Count-1 do begin
      if (ptParam(lParameters[I]).sName = sName) or ((lowercase(ptParam(lParameters[I]).sName) = lowercase(sName)) and (not bIsCaseSensitive)) then begin
        Result := I;
        Break; 
      end;
    end;
  end;

  function TCLIParser.ParseArgs(sParamString: String = ''): Integer;
  var
    I,nLen,nTemp,nCount: Integer;
    Param: ptParam;
    sCurrent,sTemp: String;
    cFirst: Char;
    bGotParam: Boolean;
    slValues: TStringList;
  begin
    bGotParam := False;
    slValues := nil;
    if (sParamString = '') then nCount := ParamCount
    else begin
      Split(' ',sParamString,slValues);
      nCount := slValues.Count;
    end;
    sCurrent := '';

    for I := 0 to nCount-1 do begin
      if (sParamString = '') then sCurrent := ParamStr(I+1)
      else sCurrent := slValues[I];

      cFirst := sCurrent[1];
      if (sCurrent[1] = '\') then sCurrent := copy(sCurrent,2,Length(sCurrent)-1);
      nLen := Length(sCurrent);
      if ((cFirst = '-') or (cFirst = '/')) and (nLen > 1) then begin
        if (bGotParam) and (Param.nCount = 1) then lParameters.Add(Param);
        bGotParam := True;
        Self.NewParam(Param);
        if (sCurrent[2] = '-') then begin
          if (nLen > 2) then sTemp := copy(sCurrent,3,nLen-2)
          else begin
            bGotParam := False;
            Self.RemoveParam(Param);
            break;
          end;
        end
        else sTemp := copy(sCurrent,2,nLen-1);
        nTemp := Self.Exists(sTemp);
        if (nTemp > -1) then begin
          Self.RemoveParam(Param);
          Param := Self.Get(nTemp);
        end
        else begin
          Param.sName := sTemp;        
          Param.sValue := '';
        end;
        inc(Param.nCount);
      end
      else if (bGotParam) then begin
        if (Param.nCount > 1) then Param.sValue := '';
        if length(Param.sValue) = 0 then Param.sValue := sCurrent
        else Param.sValue := Param.sValue+' '+sCurrent;
      end;
    end;
    if (bGotParam) and (Param.nCount = 1) then lParameters.Add(Param);
    Result := lParameters.Count;
  end;
end. 
