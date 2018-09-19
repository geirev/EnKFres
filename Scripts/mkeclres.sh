#!/usr/bin/ksh
# This function creates a list of variables used in the ECLIPSE restart files.
# The list is used for automatic generation of interface program.

cat ${1}ECLIPSE.F0??? | egrep " '"  > tmpf
for i in  REAL DOUB INTE CHAR LOGI MESS
do
   cat tmpf | egrep "'$i'"  | sed -e "s/'//g"  |\
   awk '{
      print $1, $3  
   }'  >> eclres.vars
done
cat eclres.vars | sort -u > eclres.tmp; mv eclres.tmp eclres.vars
