#!/usr/bin/ksh
# This function creates a list of variables used in the ECLIPSE summary files.
# The list is used for automatic generation of interface program.

cat ${1}ECLIPSE.A0??? | egrep " '"  > tmpf
for i in  REAL DOUB INTE CHAR LOGI MESS
do
   cat tmpf | egrep "'$i'"  | sed -e "s/'//g"  |\
   awk '{
      print $1, $3  
   }'  >> eclsum.vars
done
cat eclsum.vars | sort -u > eclsum.tmp; mv eclsum.tmp eclsum.vars
