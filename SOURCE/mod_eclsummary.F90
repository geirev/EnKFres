module mod_eclsummary
! Eclipse summary variables (ECLIPSE.A????)
   integer                 isumm
   logical              :: lsumm=.false.
   integer, parameter   :: nrsumm=3
   character(len=8)        summname(nrsumm)
   integer                 summsize(nrsumm)
   character(len=4)        summtype(nrsumm)

#include "sum_declare.inc"

contains

subroutine sumallocate()
#include "sum_allocate.inc"
end subroutine sumallocate

subroutine sumdeallocate()
#include "sum_deallocate.inc"
end subroutine sumdeallocate

end  module mod_eclsummary
