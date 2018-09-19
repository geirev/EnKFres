module mod_eclfsmspec
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Eclipse summary header variables (ECLIPSE.FSMSPEC)
   integer                 ihead
   logical              :: lhead=.false.
   integer, parameter   :: nrhead=9
   character(len=8)        headname(nrhead)
   integer                 headsize(nrhead)
   character(len=4)        headtype(nrhead)
   integer                 ikeywords,iwgnames,iunits

#include "fsm_declare.inc"

contains

subroutine fsmallocate()
#include "fsm_allocate.inc"
end subroutine fsmallocate

subroutine fsmdeallocate()
#include "fsm_deallocate.inc"
end subroutine fsmdeallocate

end  module mod_eclfsmspec
