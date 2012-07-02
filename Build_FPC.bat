@echo off
echo ---------------
echo dWOL - WOL Implementation by Dataforce.
echo Copyright (C) 2005 Shane "Dataforce" Mc Cormack
echo Released under the ZLIB License
echo For conditions of distribution and use, see copyright notice in license.txt
echo * SVN: $Id$
call Cleanup.bat
echo ---------------
echo Removing old exe
echo ---------------
del TimeStamper.exe
del dwol.exe
echo ---------------
echo Building...
echo ---------------
fpc.exe -Sd -Xs -XX -O2 -Or -Op1 TimeStamper.dpr
call TimeStamper.exe
If exist "time.inc" set includeset=%includeset% -dTimeinc
fpc.exe -Sd -Xs -XX -O2 -Or -Op1 dWOL.dpr -Fu./core/ -Fi./core/ %includeset% -odWOL_fpc.exe
echo ---------------
echo Build Complete...
echo ---------------
call Cleanup.bat