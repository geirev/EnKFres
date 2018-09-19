#!/usr/bin/ksh
# This function generates a set of include files for the interface
# program based on the list of variables contained in eclsum.vars

cat eclsum.vars |\
awk  '{
   if ($2 == "REAL") {print "    real*4,            allocatable :: ",$1"(:)"}
   if ($2 == "DOUB") {print "    double precision,  allocatable :: ",$1"(:)"}
   if ($2 == "INTE") {print "    integer,           allocatable :: ",$1"(:)"}
   if ($2 == "CHAR") {print "    character(len=10), allocatable :: ",$1"(:)"}
   if ($2 == "LOGI") {print "    logical,           allocatable :: ",$1"(:)"}
}'  > sum_declare.inc

cat eclsum.vars |\
awk  '{
   if ($2 == "REAL") {printf "    allocate( %8s(1) ); %-8s =0.0\n", $1,$1       }
   if ($2 == "DOUB") {printf "    allocate( %8s(1) ); %-8s =0.0\n", $1,$1       }
   if ($2 == "INTE") {printf "    allocate( %8s(1) ); %-8s =1  \n", $1,$1       }
   if ($2 == "LOGI") {printf "    allocate( %8s(1) ); %-8s =.false.\n", $1,$1       }
   if ($2 == "CHAR") {printf "    allocate( %8s(1) ); %-8s =#AAAAAAAAAA#\n", $1,$1       }
}'  | sed -e "s/#/'/g"   >   sum_allocate.inc

cat eclsum.vars |\
awk  '{
   if ($2 == "REAL") {printf "    deallocate( %8s )\n", $1       }
   if ($2 == "DOUB") {printf "    deallocate( %8s )\n", $1       }
   if ($2 == "INTE") {printf "    deallocate( %8s )\n", $1       }
   if ($2 == "LOGI") {printf "    deallocate( %8s )\n", $1       }
   if ($2 == "CHAR") {printf "    deallocate( %8s )\n", $1       }
}'  | sed -e "s/#/'/g"   >   sum_deallocate.inc

mv sum_*.inc ${1}/include
