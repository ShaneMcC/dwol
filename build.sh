echo ---------------
echo dWOL - WOL Implementation by Dataforce.
echo Copyright \(C\) 2005 Shane "Dataforce" Mc Cormack
echo Released under the ZLIB License
echo For conditions of distribution and use, see copyright notice in license.txt
echo SVN: \$Id:$
sh cleanup.sh
echo ---------------
echo Removing old binary
echo ---------------
rm TimeStamper
rm dWOL
echo ---------------
echo Building...
echo ---------------

fpc -Sd -Xs -XX -O2 -Or -Op1 TimeStamper.dpr
./TimeStamper
if [ -f time.inc ]
then
  fpc -Sd -Xs -XX -O2 -Or -Op1 -dTimeinc dWOL.dpr -Fu${PWD}/core/ -Fi${PWD}/core/   
else
  fpc -Sd -Xs -XX -O2 -Or -Op1 dWOL.dpr -Fu${PWD}/core/ -Fi${PWD}/core/
fi

echo ---------------
echo Build Complete
echo ---------------
sh cleanup.sh
