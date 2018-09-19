module mod_eclrestart
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Eclipse restart variables (ECLIPSE.F????)
   integer                 ifield
   integer, parameter   :: nrfields=100 ! Maximum number of fields in restart file
   integer nnfields                     ! Actual number of fields in restart file

   character(len=8)        fieldname(nrfields)
   integer                 fieldsize(nrfields)
   character(len=4)        fieldtype(nrfields)
   integer ipres,iswat,isgas,irs,irv,isomax
   integer ixgrp,irseg,ixwel,iscon,ixcon

#include "res_declare.inc"

contains

subroutine resallocate()
#include "res_allocate.inc"
end subroutine resallocate

subroutine resdeallocate()
#include "res_deallocate.inc"
end subroutine resdeallocate

integer function write_static(iens)
   use mod_fnames
   use mod_dimensions
   implicit none
   integer, intent(in) :: iens
   integer reclA
   character(len=9) rident
   inquire(iolength=reclA)reclA,rident,nrfields,fieldname,fieldsize,fieldtype,&
#include "res_iostatic.inc"
!   print *,'write_static: Required record length for member ',iens,' is record length= ',reclA

   write(13,*)'write_static: Opens static file  = ',trim(ensSfile),' with record length= ',reclA
   write(13,*)'write_static: writes file  = ',trim(ensSfile)

   open(10,file=trim(ensSfile),form='unformatted',access='direct',recl=reclA)
      rident='STATICENS'
      write(10,rec=iens)   reclA,rident,nrfields,fieldname,fieldsize,fieldtype,&
#include "res_iostatic.inc"
   close(10)
   write_static=0
end function write_static

integer function read_static(iens)
   use mod_fnames
   use mod_dimensions
   implicit none
   integer, intent(in) :: iens
   integer reclA
   integer nn
   character(len=9) rident

! Recordlength of stored static member.
   inquire(iolength=reclA)reclA,rident,nrfields,fieldname,fieldsize,fieldtype
   open(10,file=trim(ensSfile),form='unformatted',access='direct',recl=reclA)
      read(10,rec=1)reclA
   close(10)

   open(10,file=trim(ensSfile),form='unformatted',access='direct',recl=reclA)
      read(10,rec=iens)   reclA,rident,nn,fieldname,fieldsize,fieldtype,&
#include "res_iostatic.inc"
   close(10)

   read_static=0
end function read_static

end  module mod_eclrestart
