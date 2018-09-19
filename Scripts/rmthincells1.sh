#!/bin/ksh
#Script to remove small cells.
#List of small cells are stored in removed.mod

#Prepare for a referance case with one timestep to generate grid
cat SCHEDULE_orig.INC | sed -e 's/--END0001/END/g' > SCHEDULE_orig.THIN
mv SCHEDULE_orig.INC SCHEDULE_orig.ORIG
mv SCHEDULE_orig.THIN SCHEDULE_orig.INC

cat ECLIPSE_orig.DATA | sed -e 's/FMTIN/--FMTIN/g' -e 's/FMTOUT/--FMTOUT/g' > ECLIPSE_orig.THIN
mv ECLIPSE_orig.DATA ECLIPSE_orig.ORIG
mv ECLIPSE_orig.THIN ECLIPSE_orig.DATA

print "The files SCHEDULE_orig.INC and ECLIPSE_orig.DATA are updated."
print "Proceed with optin b to generate grid files."
