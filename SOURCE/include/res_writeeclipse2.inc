         case('DLYTIM')
            ird=write_double (fieldname(i),fieldsize(i),fieldtype(i),DLYTIM)

         case('DOUBHEAD')
            ird=write_double (fieldname(i),fieldsize(i),fieldtype(i),DOUBHEAD)

         case('ENDSOL')
            write(10,'(t2,a1,a8,a1,t13,i11,t25,a1,a4,a1)')&
            sep,fieldname(i),sep,fieldsize(i),sep,fieldtype(i),sep
         case('FIPFAMNA')
            ird=write_char   (fieldname(i),fieldsize(i),fieldtype(i),FIPFAMNA)

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

         case('LOGIHEAD')
            ird=write_logical(fieldname(i),fieldsize(i),fieldtype(i),LOGIHEAD)

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

         case('THRESHPR')
            ird=write_double (fieldname(i),fieldsize(i),fieldtype(i),THRESHPR)

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

