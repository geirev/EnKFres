#!/usr/bin/ksh
# This function generates a set of include files for the interface
# program based on the list of variables contained in eclsum.vars

cat eclfsm.vars |\
awk  '{
   if ($2 == "REAL") {print "    real*4,            allocatable :: ",$1"(:)"}
   if ($2 == "DOUB") {print "    double precision,  allocatable :: ",$1"(:)"}
   if ($2 == "INTE") {print "    integer,           allocatable :: ",$1"(:)"}
   if ($2 == "CHAR") {print "    character(len=10), allocatable :: ",$1"(:)"}
   if ($2 == "LOGI") {print "    logical,           allocatable :: ",$1"(:)"}
}'  > fsm_declare.inc



cat eclfsm.vars |\
awk  '{
   if ($2 == "REAL") {printf "    allocate( %8s(1) ); %-8s =0.0\n", $1,$1       }
   if ($2 == "DOUB") {printf "    allocate( %8s(1) ); %-8s =0.0\n", $1,$1       }
   if ($2 == "INTE") {printf "    allocate( %8s(1) ); %-8s =1  \n", $1,$1       }
   if ($2 == "LOGI") {printf "    allocate( %8s(1) ); %-8s =.false.\n", $1,$1       }
   if ($2 == "CHAR") {printf "    allocate( %8s(1) ); %-8s =#AAAAAAAAAA#\n", $1,$1       }
}'  | sed -e "s/#/'/g"   >   fsm_allocate.inc

cat eclfsm.vars |\
awk  '{
   if ($2 == "REAL") {printf "    deallocate( %8s )\n", $1       }
   if ($2 == "DOUB") {printf "    deallocate( %8s )\n", $1       }
   if ($2 == "INTE") {printf "    deallocate( %8s )\n", $1       }
   if ($2 == "LOGI") {printf "    deallocate( %8s )\n", $1       }
   if ($2 == "CHAR") {printf "    deallocate( %8s )\n", $1       }
}'  | sed -e "s/#/'/g"   >   fsm_deallocate.inc

cat eclfsm.vars |\
awk  '{
   printf "         case(#%s#)\n",$1
   printf "            deallocate(%s)\n",$1
   printf "            allocate(%s(headsize(ihead)))\n",$1
   if ($1 == "KEYWORDS")    {printf "            ikeywords=ihead\n"}
   if ($1 == "WGNAMES")     {printf "            iwgnames=ihead\n"}
   if ($1 == "UNITS")       {printf "            iunits=ihead\n"}
   if ($2 == "REAL") {printf "            ird=read_real(headsize(ihead),%s,headtype(ihead))\n\n",$1}
   if ($2 == "DOUB") {printf "            ird=read_double(headsize(ihead),%s,headtype(ihead))\n\n",$1}
   if ($2 == "INTE") {printf "            ird=read_integer(headsize(ihead),%s,headtype(ihead))\n\n",$1}
   if ($2 == "LOGI") {printf "            ird=read_logical(headsize(ihead),%s,headtype(ihead))\n\n",$1}
   if ($2 == "CHAR") {printf "            ird=read_char(headsize(ihead),%s,headtype(ihead))\n\n",$1}
}'  | sed -e "s/#/'/g"   > fsm_readeclipse.inc



mv fsm_*.inc ${1}/include
