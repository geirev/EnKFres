#!/usr/bin/ksh
# This function generates a set of include files for the interface
# program based on the list of variables contained in eclres.vars

cat eclres.vars | sed 's/1\//XX/' |\
awk  '{
   if ($2 == "REAL") {print "    real*4,            allocatable :: ",$1"(:)"}
   if ($2 == "DOUB") {print "    double precision,  allocatable :: ",$1"(:)"}
   if ($2 == "INTE") {print "    integer,           allocatable :: ",$1"(:)"}
   if ($2 == "CHAR") {print "    character(len=10), allocatable :: ",$1"(:)"}
   if ($2 == "LOGI") {print "    logical,           allocatable :: ",$1"(:)"}
}'  > res_declare.inc



cat eclres.vars | sed 's/1\//XX/' |\
awk  '{
   if ($2 == "REAL") {printf "    allocate( %8s(1) ); %-8s =0.0\n", $1,$1       }
   if ($2 == "DOUB") {printf "    allocate( %8s(1) ); %-8s =0.0\n", $1,$1       }
   if ($2 == "INTE") {printf "    allocate( %8s(1) ); %-8s =1  \n", $1,$1       }
   if ($2 == "LOGI") {printf "    allocate( %8s(1) ); %-8s =.false.\n", $1,$1       }
   if ($2 == "CHAR") {printf "    allocate( %8s(1) ); %-8s =#AAAAAAAAAA#\n", $1,$1       }
}'  | sed -e "s/#/'/g"   >   res_allocate.inc

cat eclres.vars | sed 's/1\//XX/' |\
awk  '{
   if ($2 == "REAL") {printf "    deallocate( %8s )\n", $1       }
   if ($2 == "DOUB") {printf "    deallocate( %8s )\n", $1       }
   if ($2 == "INTE") {printf "    deallocate( %8s )\n", $1       }
   if ($2 == "LOGI") {printf "    deallocate( %8s )\n", $1       }
   if ($2 == "CHAR") {printf "    deallocate( %8s )\n", $1       }
}'  | sed -e "s/#/'/g"   >   res_deallocate.inc


last=$(tail -1 eclres.vars | awk '{print $1}')
cat eclres.vars | sed 's/1\//XX/'  | sed -e "/PRESSURE/d" \
                         -e "/^SGAS /d" \
                         -e "/^STARTSOL /d" \
                         -e "/^ENDSOL /d" \
                         -e "/^SWAT /d" \
                         -e "/^RS /d" \
                         -e "/^RV /d" |\
awk  -v ll="$last" '{
   if ( $1 == ll )
   {
      printf "       %8s\n", $1 
   }
   else
   {
      printf "       %8s ,&\n", $1
   }
}'   > res_iostatic.inc


cat eclres.vars | sed 's/1\//XX/' |\
awk  '{
   printf "         case(#%s#)\n",$1
   if ( $2 != "MESS" ){
   printf "            deallocate(%s)\n",$1
   printf "            allocate(%s(fieldsize(i)))\n",$1
   if ($1 == "PRESSURE"){printf "            ipres=i\n"}
   if ($1 == "SGAS")    {printf "            isgas=i\n"}
   if ($1 == "SWAT")    {printf "            iswat=i\n"}
   if ($1 == "RS")      {printf "            irs  =i\n"}
   if ($1 == "RV")      {printf "            irv  =i\n"}
   }
   if ($2 == "REAL") {printf "            ird=read_real(fieldsize(i),%s,fieldtype(i))\n\n",$1}
   if ($2 == "DOUB") {printf "            ird=read_double(fieldsize(i),%s,fieldtype(i))\n\n",$1}
   if ($2 == "INTE") {printf "            ird=read_integer(fieldsize(i),%s,fieldtype(i))\n\n",$1}
   if ($2 == "LOGI") {printf "            ird=read_logical(fieldsize(i),%s,fieldtype(i))\n\n",$1}
   if ($2 == "CHAR") {printf "            ird=read_char(fieldsize(i),%s,fieldtype(i))\n\n",$1}
}'  | sed -e '/case/ s/XX/1\//' -e "s/#/'/g"   > res_readeclipse.inc


cat eclres.vars | sed 's/1\//XX/'|\
awk  '{
   printf "         case(#%s#)\n",$1
   if ( $2 != "MESS" ){
   printf "            deallocate(%s)\n",$1
   printf "            allocate(%s(fieldsize(i)))\n",$1
   if ($1 == "PRESSURE"){printf "            ipres=i\n"}
   if ($1 == "SGAS")    {printf "            isgas=i\n"}
   if ($1 == "SWAT")    {printf "            iswat=i\n"}
   if ($1 == "RS")      {printf "            irs  =i\n"}
   if ($1 == "RV")      {printf "            irv  =i\n"}
   }
}'  | sed -e '/case/ s/XX/1\//' -e "s/#/'/g"   > res_writeeclipse1.inc


cat eclres.vars | sed 's/1\//XX/' |\
awk  '{
   printf "         case(@%s@)\n",$1
   if ($2 == "REAL") {printf "            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),%s)\n\n",$1}
   if ($2 == "DOUB") {printf "            ird=write_double (fieldname(i),fieldsize(i),fieldtype(i),%s)\n\n",$1}
   if ($2 == "INTE") {printf "            ird=write_integer(fieldname(i),fieldsize(i),fieldtype(i),%s)\n\n",$1}
   if ($2 == "LOGI") {printf "            ird=write_logical(fieldname(i),fieldsize(i),fieldtype(i),%s)\n\n",$1}
   if ($2 == "CHAR") {printf "            ird=write_char   (fieldname(i),fieldsize(i),fieldtype(i),%s)\n\n",$1}
   if ($1 == "PRESSURE")
   {
                      printf "            if (iopt == 22) ird=write_real(@PERMX   @,ndim,@REAL@,PERMX)\n"
                      printf "            if (iopt == 22) ird=write_real(@PERMZ   @,ndim,@REAL@,PERMZ)\n"
                      printf "            if (iopt == 22) ird=write_real(@PORO    @,ndim,@REAL@,PORO )\n"
                      printf "#ifdef GAUSS2\n"
                      printf "            if (iopt == 22) ird=write_real(@GAUSS1  @,ndim,@REAL@,mem4++gauss1)\n"
                      printf "            if (iopt == 22) ird=write_real(@GAUSS2  @,ndim,@REAL@,mem4++gauss2)\n"
                      printf "#endif\n"
   }


   if ($1 == "STARTSOL")
   {
                      printf "            write(10,@(t2,a1,a8,a1,t13,i11,t25,a1,a4,a1)@)&\n"
                      printf "            sep,fieldname(i),sep,fieldsize(i),sep,fieldtype(i),sep\n"
   }

   if ($1 == "ENDSOL")
   {
                      printf "            write(10,@(t2,a1,a8,a1,t13,i11,t25,a1,a4,a1)@)&\n"
                      printf "            sep,fieldname(i),sep,fieldsize(i),sep,fieldtype(i),sep\n"
   }
}'  | sed -e "s/@/'/g"\
          -e "s/++/%/g"\
          -e '/case/ s/XX/1\//' > res_writeeclipse2.inc



mv res_*.inc ${1}/include

