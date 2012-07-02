unit SQL;

interface
  uses sysutils, classes, mysql;

  const
  {$IFDEF WIN32}
    TSQLDLLNAME = 'libmysql.dll';
  {$ELSE}
    TSQLDLLNAME = '/usr/lib/mysql/libmysqlclient.so';
  {$ENDIF}

  type

    TSQLInfo = record
      sDBName,sDBUser,sDBPass,sDBHost: String;
      nDBPort: Integer;
    end;

    TSQL = class
      constructor Create(AutoLoadDLL: Boolean = True);
      private
        mySQL: PMYSQL;
        bReady: Boolean;
        bGotDLL: Boolean;
        LastRes: PMYSQL_RES;
        LastField, LastRow: TStringList;
        bLastResult: Boolean;
        sLastTable: String;
        nLastInsertID: Integer;
        bAlwaysGetFirstRow: Boolean;
        sLastQuery: String;
        nQueryCount: Integer;
        pcDLLName: PChar;

        procedure NewQuery();
        procedure SetDLLName(pcNewValue: PChar);
      public
//        sHost, sUser, sPass, sDefaultDB: String;
//        nPort: Integer;
        SQLDetails: TSQLInfo;

        ReloadDLLOnChange: Boolean; // If this is true changing the DLLName will
                                    // cause the old DLL to be unloaded, and the
                                    // new one to be loaded.
                                    // If this is false, the DLLName will NOT be
                                    // changed unless there is no DLL Loaded
                                    // when called. 

        property DLLName: PChar read pcDLLName write SetDLLName;
        property AlwaysGetFirstRow: boolean read bAlwaysGetFirstRow write bAlwaysGetFirstRow;
        property DLLIsLoaded: boolean read bGotDLL;
        property IsReady: boolean read bReady;
        property LastResult: Boolean read bLastResult;
        property LastTable: String read sLastTable;
        property LastQuery: String read sLastQuery;
        property QueryCount: Integer read nQueryCount;

        procedure ResetQueryCount();
        procedure Setup(ConnectionInfo: TSQLInfo);        

        function LoadDLL(): Boolean;
        function UnLoadDLL(): Boolean;        

        function Connect(): Boolean;
        function Close(): Boolean;        
        function SelectDB(sDatabase: String = ''): Boolean;
        function Query(sQuery: String): Boolean;
        function GetNextRow(): Boolean;
        function HasRow(): Boolean;

        function RowsAffected(): Integer;
        function RowsReturned(): Integer;
        function GetFieldCount(): Integer;
        function GetInsertID(): Integer;

        function GetFieldName(nIndex: Integer): String;
        function GetFieldValue(sValue: String): String; overload;
        function GetFieldValue(nIndex: Integer): String; overload;
        function GetError(): String;
        function GetRowString(): String;
        function Escape(sInput: String; UseDLL: Boolean = False): String;
    end;


implementation

procedure TSQL.SetDLLName(pcNewValue: PChar);
begin
  if bGotDLL = False then begin
    pcDLLName := pcNewValue;
  end
  else if ReloadDLLOnChange then begin
    UnLoadDLL();
    pcDLLName := pcNewValue;
    LoadDLL();
  end;
end;

procedure TSQL.Setup(ConnectionInfo: TSQLInfo);
begin
  SQLDetails.sDBName := ConnectionInfo.sDBName;
  SQLDetails.sDBUser := ConnectionInfo.sDBUser;
  SQLDetails.sDBPass := ConnectionInfo.sDBPass;
  SQLDetails.sDBHost := ConnectionInfo.sDBHost;
  SQLDetails.nDBPort := ConnectionInfo.nDBPort;
end;

procedure TSQL.NewQuery();
begin
  if LastRes <> nil then mysql_free_result(LastRes);
  LastRes := nil;
  sLastTable := '';
  nLastInsertID := 0;
  LastRow.Clear;
  LastField.Clear;
  sLastQuery := '';  
end;

function TSQL.UnLoadDLL(): Boolean;
begin
  bGotDLL := False;
  Result := True;  
  try
    libmysql_free;
  except
    Result := False;
  end;
end;

function TSQL.LoadDLL(): Boolean;
{$IFNDEF WIN32}
var
  TryLocal: Boolean;
{$ENDIF}
begin
  if bGotDLL then exit; // Don't Load if DLL is already loaded.
  {$IFNDEF WIN32}TryLocal := False;{$ENDIF}
  if Self.DLLName = '' then begin
    Self.DLLName := TSQLDLLNAME;
    {$IFNDEF WIN32}TryLocal := True;{$ENDIF}
  end;
  try
    if fileexists(Self.DLLName) then begin
      libmysql_load(Self.DLLName);
//      WriteLN('Status: '+inttostr(libmysql_status));
      if (libmysql_status = LIBMYSQL_READY) then begin
        MySQL := mysql_init(nil);
        bGotDLL := True;
      end;
    {$IFNDEF WIN32}
    end
    else if TryLocal and fileexists('./libmysqlclient.so') then begin
      libmysql_load('./libmysqlclient.so');
      if (libmysql_status = LIBMYSQL_READY) then begin
        MySQL := mysql_init(nil);
        bGotDLL := True;
        Self.DLLName := './libmysqlclient.so';
      end;
    {$ENDIF}
    end;
  except
  end;
  Result := bGotDLL;
end;

constructor TSQL.Create(AutoLoadDLL: Boolean = True);
begin
  SQLDetails.nDBPort := 0;
  mySQL := nil;
  ReloadDLLOnChange := False;
  bReady := False;
  bGotDLL := False;
  MySQL := nil;
  LastField := TStringList.Create();
  LastRow := TStringList.Create();
  bAlwaysGetFirstRow := False;
  if AutoLoadDLL then Self.LoadDLL();
  NewQuery();
  ResetQueryCount();
end;

function TSQL.Close(): Boolean;
begin
  Result := False;
  if (not bGotDLL) then exit;
  NewQuery();  
  mysql_close(mySQL);
  Result := True;
  bReady := False;
end;

function TSQL.Connect(): Boolean;
begin
  Result := False;
  if (not bGotDLL) then exit;
  NewQuery();
  if mysql_real_connect(mySQL, pChar(SQLDetails.sDBHost), pChar(SQLDetails.sDBUser), pChar(SQLDetails.sDBPass), pChar(SQLDetails.sDBName), SQLDetails.nDBPort, nil, 0) <> nil then Result := True;
  bReady := Result;
end;

function TSQL.SelectDB(sDatabase: String = ''): Boolean;
begin
  Result := False;
  if (not bGotDLL) then exit;
  NewQuery();  
  if (not bReady) then exit;
  if sDatabase = '' then sDatabase := SQLDetails.sDBName;
  if mysql_select_db(mySQL, pChar(sDatabase)) = 0 then Result := True;
end;


function TSQL.GetError(): String;
begin
  if (not bGotDLL) then Result := '-1 - libmySQL.dll not loaded'
  else Result := Format('%d - %s', [mysql_errno(MySQL), mysql_error(MySQL)]);
end;

function TSQL.Query(sQuery: String): Boolean;
var
  Field: PMYSQL_FIELD;
begin
//  WriteLN('Query: '+sQuery);
  Result := False;
  bLastResult := Result;
  if (not bGotDLL) then exit;
  NewQuery();
  sLastQuery := sQuery;
  inc(nQueryCount);
  if mysql_query(MySQL, pChar(sQuery)) = 0 then begin
    nLastInsertID := MySQL_Insert_ID(MySQL);
    Result := True;
    LastRes := mysql_use_result(MySQL);
    if LastRes = nil then exit;
    Field := mysql_fetch_field(LastRes);
    sLastTable := '';
    while Field <> nil do begin
      if sLastTable = '' then sLastTable := Field.table;
      LastField.Add(Field.name);
      Field := mysql_fetch_field(LastRes);
    end;
    if bAlwaysGetFirstRow then GetNextRow;
  end;
  bLastResult := Result;
//  WriteLN('Error: '+Self.GetError);  
end;

function TSQL.RowsAffected(): Integer;
begin
  Result := mysql_affected_rows(MySQL);
end;

function TSQL.RowsReturned(): Integer;
begin
  Result := 0;
  if LastRes = nil then exit;
  Result := mysql_num_rows(LastRes);
end;

function TSQL.GetFieldCount(): Integer;
begin
  Result := LastField.Count;
end;

function TSQL.GetFieldName(nIndex: Integer): String;
begin
  Result := '';
  if nIndex < 0 then exit;
  if (nIndex < LastField.count) then Result := LastField[nIndex];
end;

function TSQL.GetFieldValue(nIndex: Integer): String;
begin
  Result := '';
  if (nIndex < LastRow.count) then Result := LastRow[nIndex];
end;                                              
function TSQL.GetFieldValue(sValue: String): String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to LastRow.count-1 do begin
    if lowercase(sValue) = lowercase(LastField[I]) then begin
      Result := LastRow[I];
      break;
    end;
  end;
end;

function TSQL.GetInsertID(): Integer;
begin
  Result := nLastInsertID;
end;

function TSQL.HasRow(): Boolean;
begin
  if LastRow.Count > 0 then result := True
  else Result := False;
end;

function TSQL.GetRowString(): String;
var
  I: Integer;
begin
  Result := '';
  if not HasRow then begin
    Result := 'No Row To Print';
  end
  else begin
    if LastRow.count <> LastField.Count then begin
      Result := 'Row Consistancy Error';    
    end
    else begin
      for I := 0 to LastRow.count-1 do begin
        if Result <> '' then Result := Result+' | ';
        Result := Result+LastField[I]+': '+LastRow[I];
      end;
    end;
  end;
end;
        
function TSQL.GetNextRow(): Boolean;
var
  Row: PMYSQL_ROW;
  Lengths: PMYSQL_LENGTHS;
  I: Integer;
  Buffer: String;
begin
  LastRow.Clear;
  Result := False;
  bLastResult := Result;
  if (not bGotDLL) then exit;
  if (LastRes = nil) then exit;  

  Row := mysql_fetch_row(LastRes);
  if Row = nil then exit;
  Lengths := mysql_fetch_lengths(LastRes);
  if Lengths = nil then exit;
  for I := 0 to GetFieldCount-1 do begin
    SetString(Buffer, Row[I], Lengths[I]);
    copy(Buffer, 1, mysql_fetch_field_direct(LastRes, I).length);
    LastRow.Add(Buffer);
  end;
  Result := True;
end;

procedure TSQL.ResetQueryCount();
begin
  nQueryCount := 0;
end;

function TSQL.Escape(sInput: String; UseDLL: Boolean = False): String;
var
  pResult: PChar;
  i: Integer;
  c: Char;
begin
  // DLL Method is broken atm, need to take a look
  if UseDLL then begin
    GetMem(pResult,length(sInput)+1);
    mysql_real_escape_string(MySQL,pResult,PChar(sInput),Length(sInput));
    Result := PChar(pResult);
    FreeMem(pResult);
  end
  else begin
    for I := 1 to length(sInput) do begin
      C := sInput[I];
      case ord(C) of
        0: Result := Result+'\0';   // NUL
        10: Result := Result+'\n';  // New Line
        13: Result := Result+'\r';  // Line Fedd
        26: Result := Result+'\Z';  // Control-Z
        34: Result := Result+'\"';  // "
        39: Result := Result+'\'''; // '
        92: Result := Result+'\\';  // \
      else
        Result := Result+C;
      end;
    end;
  end;
end;

end.
