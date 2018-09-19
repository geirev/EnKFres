#!/bin/ksh
#The grid must have been generated, that is the files ECLIPSE.GRID, ECLIPSE.EGRID must be stored in the Refcase directory

cd Refcase

print PROCESS INIT DATA             > thin_remove.eclp
print REMOVE THIN GRID BLOCKS      >> thin_remove.eclp
print ECLIPSE                      >> thin_remove.eclp
print tb_removed                   >> thin_remove.eclp
#print YES                          >> thin_remove.eclp
print ALL GRID BLOCKS              >> thin_remove.eclp
print "Remove all cells with thikness less than (0.1) m: "
read  thickness
print $thickness                    >> thin_remove.eclp
#print .1                           >> thin_remove.eclp
print 99999999                     >> thin_remove.eclp
print CONTINUE AND WRITE RESULTS   >> thin_remove.eclp

#make a list of cells to be removed, stored in tb_removed.mod
eclpost thin_remove.eclp

#modify the list of small cells and store it in removed.mod

sed -e "s/'MINPVV  '  .\{8\}/MINPVV   99999999/g" tb_removed.mod > removed.mod

cd ..

mv Refcase/removed.mod .

#cleaning up
if [ -r SCHEDULE_orig.ORIG -a -r ECLIPSE_orig.ORIG ]
then
   mv SCHEDULE_orig.ORIG SCHEDULE_orig.INC 
   mv ECLIPSE_orig.ORIG  ECLIPSE_orig.DATA
fi

print "Make sure the new removed.mod file is checked and properly included in ECLIPSE_orig.DATA"
