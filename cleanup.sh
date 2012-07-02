echo ---------------
echo Cleaning...
echo SVN: \$Id: cleanup.sh 145 2005-10-30 21:23:14Z Dataforce $
echo ---------------
echo Removing .dcu files...
rm ./*.dcu
rm ./core/*.dcu
echo Removing .o and .ppu files...
rm ./*.o
rm ./*.ppu
rm ./core/*.o
rm ./core/*.ppu
echo Removing backup files...
rm ./*.~*
echo Removing misc unneeded files...
rm ./*.or
rm ./*.ddp
rm ./*.drc
rm ./*.map
rm ./*.rsm