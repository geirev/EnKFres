         case('1/FVFGAS')
            deallocate(XXFVFGAS)
            allocate(XXFVFGAS(fieldsize(i)))
            ird=read_real(fieldsize(i),XXFVFGAS,fieldtype(i))

         case('1/FVFOIL')
            deallocate(XXFVFOIL)
            allocate(XXFVFOIL(fieldsize(i)))
            ird=read_real(fieldsize(i),XXFVFOIL,fieldtype(i))

         case('1/FVFWAT')
            deallocate(XXFVFWAT)
            allocate(XXFVFWAT(fieldsize(i)))
            ird=read_real(fieldsize(i),XXFVFWAT,fieldtype(i))

         case('AMATRIX')
            deallocate(AMATRIX)
            allocate(AMATRIX(fieldsize(i)))
            ird=read_real(fieldsize(i),AMATRIX,fieldtype(i))

         case('DBGPO')
            deallocate(DBGPO)
            allocate(DBGPO(fieldsize(i)))
            ird=read_real(fieldsize(i),DBGPO,fieldtype(i))

         case('DBOPO')
            deallocate(DBOPO)
            allocate(DBOPO(fieldsize(i)))
            ird=read_real(fieldsize(i),DBOPO,fieldtype(i))

         case('DBWPO')
            deallocate(DBWPO)
            allocate(DBWPO(fieldsize(i)))
            ird=read_real(fieldsize(i),DBWPO,fieldtype(i))

         case('DLYTIM')
            deallocate(DLYTIM)
            allocate(DLYTIM(fieldsize(i)))
            ird=read_double(fieldsize(i),DLYTIM,fieldtype(i))

         case('DOUBHEAD')
            deallocate(DOUBHEAD)
            allocate(DOUBHEAD(fieldsize(i)))
            ird=read_double(fieldsize(i),DOUBHEAD,fieldtype(i))

         case('DRSSP')
            deallocate(DRSSP)
            allocate(DRSSP(fieldsize(i)))
            ird=read_real(fieldsize(i),DRSSP,fieldtype(i))

         case('ENDSOL')
         case('FIPFAMNA')
            deallocate(FIPFAMNA)
            allocate(FIPFAMNA(fieldsize(i)))
            ird=read_char(fieldsize(i),FIPFAMNA,fieldtype(i))

         case('FIPGAS')
            deallocate(FIPGAS)
            allocate(FIPGAS(fieldsize(i)))
            ird=read_real(fieldsize(i),FIPGAS,fieldtype(i))

         case('FIPOIL')
            deallocate(FIPOIL)
            allocate(FIPOIL(fieldsize(i)))
            ird=read_real(fieldsize(i),FIPOIL,fieldtype(i))

         case('FIPWAT')
            deallocate(FIPWAT)
            allocate(FIPWAT(fieldsize(i)))
            ird=read_real(fieldsize(i),FIPWAT,fieldtype(i))

         case('GAS_DEN')
            deallocate(GAS_DEN)
            allocate(GAS_DEN(fieldsize(i)))
            ird=read_real(fieldsize(i),GAS_DEN,fieldtype(i))

         case('GASKR')
            deallocate(GASKR)
            allocate(GASKR(fieldsize(i)))
            ird=read_real(fieldsize(i),GASKR,fieldtype(i))

         case('GASQUIES')
            deallocate(GASQUIES)
            allocate(GASQUIES(fieldsize(i)))
            ird=read_real(fieldsize(i),GASQUIES,fieldtype(i))

         case('GAS_VISC')
            deallocate(GAS_VISC)
            allocate(GAS_VISC(fieldsize(i)))
            ird=read_real(fieldsize(i),GAS_VISC,fieldtype(i))

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

         case('IWLS')
            deallocate(IWLS)
            allocate(IWLS(fieldsize(i)))
            ird=read_integer(fieldsize(i),IWLS,fieldtype(i))

         case('LOGIHEAD')
            deallocate(LOGIHEAD)
            allocate(LOGIHEAD(fieldsize(i)))
            ird=read_logical(fieldsize(i),LOGIHEAD,fieldtype(i))

         case('OIL_DEN')
            deallocate(OIL_DEN)
            allocate(OIL_DEN(fieldsize(i)))
            ird=read_real(fieldsize(i),OIL_DEN,fieldtype(i))

         case('OILKR')
            deallocate(OILKR)
            allocate(OILKR(fieldsize(i)))
            ird=read_real(fieldsize(i),OILKR,fieldtype(i))

         case('OIL_VISC')
            deallocate(OIL_VISC)
            allocate(OIL_VISC(fieldsize(i)))
            ird=read_real(fieldsize(i),OIL_VISC,fieldtype(i))

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

         case('SFIPGAS')
            deallocate(SFIPGAS)
            allocate(SFIPGAS(fieldsize(i)))
            ird=read_real(fieldsize(i),SFIPGAS,fieldtype(i))

         case('SFIPOIL')
            deallocate(SFIPOIL)
            allocate(SFIPOIL(fieldsize(i)))
            ird=read_real(fieldsize(i),SFIPOIL,fieldtype(i))

         case('SFIPWAT')
            deallocate(SFIPWAT)
            allocate(SFIPWAT(fieldsize(i)))
            ird=read_real(fieldsize(i),SFIPWAT,fieldtype(i))

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

         case('WAT_DEN')
            deallocate(WAT_DEN)
            allocate(WAT_DEN(fieldsize(i)))
            ird=read_real(fieldsize(i),WAT_DEN,fieldtype(i))

         case('WATKR')
            deallocate(WATKR)
            allocate(WATKR(fieldsize(i)))
            ird=read_real(fieldsize(i),WATKR,fieldtype(i))

         case('WATQUIES')
            deallocate(WATQUIES)
            allocate(WATQUIES(fieldsize(i)))
            ird=read_real(fieldsize(i),WATQUIES,fieldtype(i))

         case('WAT_VISC')
            deallocate(WAT_VISC)
            allocate(WAT_VISC(fieldsize(i)))
            ird=read_real(fieldsize(i),WAT_VISC,fieldtype(i))

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

         case('ZWLS')
            deallocate(ZWLS)
            allocate(ZWLS(fieldsize(i)))
            ird=read_char(fieldsize(i),ZWLS,fieldtype(i))

