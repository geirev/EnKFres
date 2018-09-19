         case('DIMENS')
            deallocate(DIMENS)
            allocate(DIMENS(headsize(ihead)))
            ird=read_integer(headsize(ihead),DIMENS,headtype(ihead))

         case('KEYWORDS')
            deallocate(KEYWORDS)
            allocate(KEYWORDS(headsize(ihead)))
            ikeywords=ihead
            ird=read_char(headsize(ihead),KEYWORDS,headtype(ihead))

         case('NUMS')
            deallocate(NUMS)
            allocate(NUMS(headsize(ihead)))
            ird=read_integer(headsize(ihead),NUMS,headtype(ihead))

         case('RESTART')
            deallocate(RESTART)
            allocate(RESTART(headsize(ihead)))
            ird=read_char(headsize(ihead),RESTART,headtype(ihead))

         case('STARTDAT')
            deallocate(STARTDAT)
            allocate(STARTDAT(headsize(ihead)))
            ird=read_integer(headsize(ihead),STARTDAT,headtype(ihead))

         case('UNITS')
            deallocate(UNITS)
            allocate(UNITS(headsize(ihead)))
            iunits=ihead
            ird=read_char(headsize(ihead),UNITS,headtype(ihead))

         case('WGNAMES')
            deallocate(WGNAMES)
            allocate(WGNAMES(headsize(ihead)))
            iwgnames=ihead
            ird=read_char(headsize(ihead),WGNAMES,headtype(ihead))

