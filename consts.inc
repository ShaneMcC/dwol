// SVN: $Id:$
  DWOL_VERSION = 'dWOL0.0';
  CODENAME = 'Genesis';
  {$DEFINE CodeName} // Use codename
  {$DEFINE DevVersion} // Is development?
  // {$DEFINE Testing} // Is pre-release?
  PASSWORD_SALT = 'p7K2j4P3s4X6v9E7t8I9b8E2c3P6q4A6e8E0q4Z7';

  {$I icsdefs.inc}
  {$IFDEF WIN32}
    DWOL_OS = 'Win32';
    {$IFDEF FPC}
      DWOL_COMPILER = 'FPC';
    {$ELSE}
      {$IFDEF DELPHI3}
        DWOL_COMPILER = 'Delphi 3';
      {$ELSE}
        {$IFDEF DELPHI4}
          DWOL_COMPILER = 'Delphi 4';
        {$ELSE}
          {$IFDEF DELPHI5}
            DWOL_COMPILER = 'Delphi 5';
          {$ELSE}
            {$IFDEF DELPHI6}
              DWOL_COMPILER = 'Delphi 6';
            {$ELSE}
             {$IFDEF DELPHI9_UP}
               DWOL_COMPILER = 'Delphi 9 or Above';
              {$ELSE}
               {$IFDEF DELPHI8_UP}
                 DWOL_COMPILER = 'Delphi 8';
                {$ELSE}
                 {$IFDEF DELPHI7_UP}
                   DWOL_COMPILER = 'Delphi 7';
                  {$ELSE}
                   DWOL_COMPILER = 'Unknown';
                  {$ENDIF}
                {$ENDIF}
              {$ENDIF}
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ELSE}
    DWOL_OS = 'Unix';
    {$IFDEF FPC}
      DWOL_COMPILER = 'FPC';
    {$ELSE}
      {$ifdef VER150}
        DWOL_COMPILER = 'Kylix 3';
      {$else}
        {$ifdef VER140}
          {$ifdef conditionalexpressions}
            {$if Declared(RTLVersion) and (RTLVersion = 14)}
              DWOL_COMPILER = 'Kylix 1';
            {$else}
              DWOL_COMPILER = 'Kylix 2';
            {$ifend}
          {$else}
            DWOL_COMPILER = 'Kylix 1 *';
          {$endif}
        {$else}
          DWOL_COMPILER = 'Unknown';
        {$endif}
      {$endif}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF Timeinc}
    {$I time.inc}
  {$ELSE}
    BUILDTIME = 0;
  {$ENDIF}

  VERSION_STRING = DWOL_VERSION+{$IFDEF Testing}'-testing'+{$ELSE}{$IFDEF DevVersion}'-devel'+{$ENDIF}{$ENDIF}{$IFDEF CodeName}' ('+CODENAME+')'+{$ENDIF}' (OS: '+DWOL_OS+') (Compiler: '+DWOL_COMPILER+')';
