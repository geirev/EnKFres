module m_ensio
integer reclmem
contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Function for opening and verifying an ensemble file

integer function open_ensemblefile(ensfile,status)
use mod_states
implicit none
character(len=*), intent(in) :: ensfile
character(len=3), intent(in) :: status
integer nxx,nyy,nzz,nww,reclB
character(len=9) rident
logical ex,lopen
type(states4) mem4

! First see if file exists
   inquire(file=trim(ensfile),exist=ex)
   if (status == 'old' .or. status =='OLD') then
      if (.not.ex) then
         print *,'The file ',trim(ensfile),' does not exist.'
         stop
      endif
   elseif (status == 'new' .or. status =='NEW') then
      if (ex) then
         print *,'The file ',trim(ensfile),' already exist and cannot be created.'
         stop
      endif
   elseif (status == 'unknown' .or. status =='UNKNOWN') then
      if (ex) print '(2a)','Opening existing ensemble file: ',trim(ensfile)
      if (.not.ex) print '(2a)','Creating new ensemble file',trim(ensfile)
   endif

   inquire(iolength=reclmem)rident,reclmem,nx,ny,nz,nw,&  
#include "memlist.inc"

   inquire(unit=10,opened=lopen)
   if (lopen) stop 'open_ensemblefile: unit 10 is already in use'

   open(10,file=trim(ensfile),form='unformatted',access='direct',recl=reclmem)
!      print '(3a)','ensio, file opened: +++',trim(ensfile),'+++'
      
      if (status == 'old' .or. status =='OLD') then
         read(10,rec=1,err=90)rident,reclB,nxx,nyy,nzz,nww

         if (reclB /= reclmem .or. nxx /= nx .or. nyy /= ny .or. nzz /= nz .or. nww /= nw) then
            print *
            print '(3a)','ERROR (ensio): ', trim(ensfile),' is not consistent with definitions'
            print *,rident, reclB, reclmem, nxx, nx, nyy, ny, nzz, nz, nww, nw
            stop
         endif
      endif

      open_ensemblefile=0

      return

      90 stop 'open_ensemblefile: weird error in read'

end function open_ensemblefile



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Function for reading an ensemble member

integer function read_ensmem(iens,mem4)
use mod_states
implicit none
integer,          intent(in)  :: iens
type (states4),   intent(out) :: mem4
integer nxx,nyy,nzz,nww,reclB,i
character(len=9) rident

   read(10,rec=iens,err=90)rident,reclB,nxx,nyy,nzz,nww,&
#include "memlist.inc"

   read_ensmem=0

   return
   90 stop 'read_ensmem: weird error in read_ensmem'

end function read_ensmem


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Function for writing an ensemble member

integer function write_ensmem(iens,rident,mem4)
use mod_states
implicit none
integer,          intent(in)  :: iens
character(len=9), intent(in)  :: rident
type (states4),   intent(in)  :: mem4
integer reclB,i

   write(10,rec=iens,err=90)rident,reclmem,nx,ny,nz,nw,&
#include "memlist.inc"

   write_ensmem=0

   return
   90 stop 'write_ensmem: weird error in write_ensmem, unit 10 free?'

end function write_ensmem


end module m_ensio
