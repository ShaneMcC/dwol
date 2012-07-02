@echo off
echo ---------------
echo Cleaning...
echo * SVN: $Id:$
echo ---------------
echo Removing DCU files...
del *.dcu
del core/*.dcu
echo Removing .o and .ppu files...
del *.o
del *.ppu
del core/*.o
del core/*.ppu
echo Removing backup files...
del *.~*
echo Removing misc unneeded files...
del *.or
del *.ddp
del *.drc
del *.map
del *.rsm
echo ---------------
echo Done.
echo ---------------