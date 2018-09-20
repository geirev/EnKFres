export LC_ALL=C
egrep -v '(^C|^c|^!)' *.F90 |\
grep " function" |\
sed -e '/[eE]nd [ ]*function/d'  -e 's/:/          /g' -e 's/integer//' -e 's/real//' -e 's/character//' |\
cut -f1 -d "(" |\
awk '{
   print $3"	"$1"	/" $2" "$3}' > tagsC

egrep -v '(^C|^c|^!)' *.F90 |\
grep subroutine |\
cut -f1 -d"(" |\
sed -e '/[eE]nd [ ]*subroutine/d' -e 's/:/	/g'  |\
awk '{
   print $3"	"$1"	/" $2" "$3}' > tagsA

egrep -v '(^C|^c|^!)' *.F90 |\
grep module |\
cut -f1 -d"(" |\
sed -e '/[eE]nd [ ]*module/d' -e 's/:/	/g' -e "/procedure/d" |\
awk '{
   print $3"	"$1"	/" $2" "$3}'  > tagsB

cat tagsA tagsB tagsC | sort -u > tags
rm tagsA tagsB tagsC
