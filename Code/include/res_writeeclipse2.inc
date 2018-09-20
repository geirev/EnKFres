         case('1/FVFGAS')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),XXFVFGAS)

         case('1/FVFOIL')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),XXFVFOIL)

         case('1/FVFWAT')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),XXFVFWAT)

         case('AMATRIX')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),AMATRIX)

         case('DBGPO')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),DBGPO)

         case('DBOPO')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),DBOPO)

         case('DBWPO')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),DBWPO)

         case('DLYTIM')
            ird=write_double (fieldname(i),fieldsize(i),fieldtype(i),DLYTIM)

         case('DOUBHEAD')
            ird=write_double (fieldname(i),fieldsize(i),fieldtype(i),DOUBHEAD)

         case('DRSSP')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),DRSSP)

         case('ENDSOL')
            write(10,'(t2,a1,a8,a1,t13,i11,t25,a1,a4,a1)')&
            sep,fieldname(i),sep,fieldsize(i),sep,fieldtype(i),sep
         case('FIPFAMNA')
            ird=write_char   (fieldname(i),fieldsize(i),fieldtype(i),FIPFAMNA)

         case('FIPGAS')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),FIPGAS)

         case('FIPOIL')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),FIPOIL)

         case('FIPWAT')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),FIPWAT)

         case('GAS_DEN')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),GAS_DEN)

         case('GASKR')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),GASKR)

         case('GASQUIES')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),GASQUIES)

         case('GAS_VISC')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),GAS_VISC)

         case('HIDDEN')
            ird=write_char   (fieldname(i),fieldsize(i),fieldtype(i),HIDDEN)

         case('ICON')
            ird=write_integer(fieldname(i),fieldsize(i),fieldtype(i),ICON)

         case('IGRP')
            ird=write_integer(fieldname(i),fieldsize(i),fieldtype(i),IGRP)

         case('INTEHEAD')
            ird=write_integer(fieldname(i),fieldsize(i),fieldtype(i),INTEHEAD)

         case('IWEL')
            ird=write_integer(fieldname(i),fieldsize(i),fieldtype(i),IWEL)

         case('IWLS')
            ird=write_integer(fieldname(i),fieldsize(i),fieldtype(i),IWLS)

         case('LOGIHEAD')
            ird=write_logical(fieldname(i),fieldsize(i),fieldtype(i),LOGIHEAD)

         case('OIL_DEN')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),OIL_DEN)

         case('OILKR')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),OILKR)

         case('OIL_VISC')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),OIL_VISC)

         case('PRESSURE')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),PRESSURE)

            if (iopt == 22) ird=write_real('PERMX   ',ndim,'REAL',PERMX)
            if (iopt == 22) ird=write_real('PERMZ   ',ndim,'REAL',PERMZ)
            if (iopt == 22) ird=write_real('PORO    ',ndim,'REAL',PORO )
#ifdef GAUSS2
            if (iopt == 22) ird=write_real('GAUSS1  ',ndim,'REAL',mem4%gauss1)
            if (iopt == 22) ird=write_real('GAUSS2  ',ndim,'REAL',mem4%gauss2)
#endif
         case('REGDIMS')
            ird=write_integer(fieldname(i),fieldsize(i),fieldtype(i),REGDIMS)

         case('REGRPT')
            ird=write_double (fieldname(i),fieldsize(i),fieldtype(i),REGRPT)

         case('RS')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),RS)

         case('RV')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),RV)

         case('SCON')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),SCON)

         case('SFIPGAS')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),SFIPGAS)

         case('SFIPOIL')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),SFIPOIL)

         case('SFIPWAT')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),SFIPWAT)

         case('SGAS')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),SGAS)

         case('SGRP')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),SGRP)

         case('SOMAX')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),SOMAX)

         case('STARTSOL')
            write(10,'(t2,a1,a8,a1,t13,i11,t25,a1,a4,a1)')&
            sep,fieldname(i),sep,fieldsize(i),sep,fieldtype(i),sep
         case('SWAT')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),SWAT)

         case('SWEL')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),SWEL)

         case('WAT_DEN')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),WAT_DEN)

         case('WATKR')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),WATKR)

         case('WATQUIES')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),WATQUIES)

         case('WAT_VISC')
            ird=write_real   (fieldname(i),fieldsize(i),fieldtype(i),WAT_VISC)

         case('XCON')
            ird=write_double (fieldname(i),fieldsize(i),fieldtype(i),XCON)

         case('XGRP')
            ird=write_double (fieldname(i),fieldsize(i),fieldtype(i),XGRP)

         case('XWEL')
            ird=write_double (fieldname(i),fieldsize(i),fieldtype(i),XWEL)

         case('ZGRP')
            ird=write_char   (fieldname(i),fieldsize(i),fieldtype(i),ZGRP)

         case('ZWEL')
            ird=write_char   (fieldname(i),fieldsize(i),fieldtype(i),ZWEL)

         case('ZWLS')
            ird=write_char   (fieldname(i),fieldsize(i),fieldtype(i),ZWLS)

