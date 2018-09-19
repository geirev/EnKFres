         case('DLYTIM')
            deallocate(DLYTIM)
            allocate(DLYTIM(fieldsize(i)))
            ird=read_double(fieldsize(i),DLYTIM,fieldtype(i))

         case('DOUBHEAD')
            deallocate(DOUBHEAD)
            allocate(DOUBHEAD(fieldsize(i)))
            ird=read_double(fieldsize(i),DOUBHEAD,fieldtype(i))

         case('ENDSOL')
         case('FIPFAMNA')
            deallocate(FIPFAMNA)
            allocate(FIPFAMNA(fieldsize(i)))
            ird=read_char(fieldsize(i),FIPFAMNA,fieldtype(i))

         case('HIDDEN')
            deallocate(HIDDEN)
            allocate(HIDDEN(fieldsize(i)))
            ird=read_char(fieldsize(i),HIDDEN,fieldtype(i))

         case('ICON')
            deallocate(ICON)
            allocate(ICON(fieldsize(i)))
            ird=read_integer(fieldsize(i),ICON,fieldtype(i))

         case('IGRP')
            deallocate(IGRP)
            allocate(IGRP(fieldsize(i)))
            ird=read_integer(fieldsize(i),IGRP,fieldtype(i))

         case('INTEHEAD')
            deallocate(INTEHEAD)
            allocate(INTEHEAD(fieldsize(i)))
            ird=read_integer(fieldsize(i),INTEHEAD,fieldtype(i))

         case('IWEL')
            deallocate(IWEL)
            allocate(IWEL(fieldsize(i)))
            ird=read_integer(fieldsize(i),IWEL,fieldtype(i))

         case('LOGIHEAD')
            deallocate(LOGIHEAD)
            allocate(LOGIHEAD(fieldsize(i)))
            ird=read_logical(fieldsize(i),LOGIHEAD,fieldtype(i))

         case('PRESSURE')
            deallocate(PRESSURE)
            allocate(PRESSURE(fieldsize(i)))
            ipres=i
            ird=read_real(fieldsize(i),PRESSURE,fieldtype(i))

         case('REGDIMS')
            deallocate(REGDIMS)
            allocate(REGDIMS(fieldsize(i)))
            ird=read_integer(fieldsize(i),REGDIMS,fieldtype(i))

         case('REGRPT')
            deallocate(REGRPT)
            allocate(REGRPT(fieldsize(i)))
            ird=read_double(fieldsize(i),REGRPT,fieldtype(i))

         case('RS')
            deallocate(RS)
            allocate(RS(fieldsize(i)))
            irs  =i
            ird=read_real(fieldsize(i),RS,fieldtype(i))

         case('RV')
            deallocate(RV)
            allocate(RV(fieldsize(i)))
            irv  =i
            ird=read_real(fieldsize(i),RV,fieldtype(i))

         case('SCON')
            deallocate(SCON)
            allocate(SCON(fieldsize(i)))
            ird=read_real(fieldsize(i),SCON,fieldtype(i))

         case('SGAS')
            deallocate(SGAS)
            allocate(SGAS(fieldsize(i)))
            isgas=i
            ird=read_real(fieldsize(i),SGAS,fieldtype(i))

         case('SGRP')
            deallocate(SGRP)
            allocate(SGRP(fieldsize(i)))
            ird=read_real(fieldsize(i),SGRP,fieldtype(i))

         case('SOMAX')
            deallocate(SOMAX)
            allocate(SOMAX(fieldsize(i)))
            ird=read_real(fieldsize(i),SOMAX,fieldtype(i))

         case('STARTSOL')
         case('SWAT')
            deallocate(SWAT)
            allocate(SWAT(fieldsize(i)))
            iswat=i
            ird=read_real(fieldsize(i),SWAT,fieldtype(i))

         case('SWEL')
            deallocate(SWEL)
            allocate(SWEL(fieldsize(i)))
            ird=read_real(fieldsize(i),SWEL,fieldtype(i))

         case('THRESHPR')
            deallocate(THRESHPR)
            allocate(THRESHPR(fieldsize(i)))
            ird=read_double(fieldsize(i),THRESHPR,fieldtype(i))

         case('XCON')
            deallocate(XCON)
            allocate(XCON(fieldsize(i)))
            ird=read_double(fieldsize(i),XCON,fieldtype(i))

         case('XGRP')
            deallocate(XGRP)
            allocate(XGRP(fieldsize(i)))
            ird=read_double(fieldsize(i),XGRP,fieldtype(i))

         case('XWEL')
            deallocate(XWEL)
            allocate(XWEL(fieldsize(i)))
            ird=read_double(fieldsize(i),XWEL,fieldtype(i))

         case('ZGRP')
            deallocate(ZGRP)
            allocate(ZGRP(fieldsize(i)))
            ird=read_char(fieldsize(i),ZGRP,fieldtype(i))

         case('ZWEL')
            deallocate(ZWEL)
            allocate(ZWEL(fieldsize(i)))
            ird=read_char(fieldsize(i),ZWEL,fieldtype(i))

