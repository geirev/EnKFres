module m_sm_iniensstat
contains
subroutine sm_iniensstat()
use mod_states
use mod_localdefs
use m_printhelp
use m_ensstat
implicit none
character(len=2) comm
character(len=100) command
character(len=80) fileold
character(len=80) filenew
character(len=80) filename
integer i,j,iflag
character(len=1) yn
logical ex,lopen

comm=' '
do 
   print '(a)','    ======================================================='
   print '(a)','    = Compute statistics of initial ensemble              ='
   print '(a)','    ======================================================='
   print '(a)','    =   a: statistics of (ensbase)F0000.uf                ='
   print '(a)','    =   b: statistics of (ensbase)A0000.uf                ='
   print '(a)','    =   h: help                                           ='
   print '(a)','    =   q: return to main menu                            ='
   print '(a)','    ======================================================='
   write(*,'(a)',advance='no')'Command: '
   read(*,*)comm

   select case (trim(comm))
   case('q')
      exit

   case('a')
      filename(:)=' '
      i=len_trim(ensbase)
      filename(1:i)=trim(ensbase)
      filename(i+1:i+8)='F0000.uf'
      inquire(unit=10,opened=lopen)
      if (lopen) close(10)
      open(10,file='ensstat.files')
         write(10,'(a)')trim(filename)
      close(10)
      iflag=1
      call ensstat(iflag)

   case('b')
      filename(:)=' '
      i=len_trim(ensbase)
      filename(1:i)=trim(ensbase)
      filename(i+1:i+8)='A0000.uf'
      inquire(unit=10,opened=lopen)
      if (lopen) close(10)
      open(10,file='ensstat.files')
         write(10,'(a)')trim(filename)
      close(10)
      iflag=1
      call ensstat(iflag)

   case('h')
      call printhelp('iniensstat.txt')

   case default
      print '(a)','Invalid command: try again!'
   end select

enddo

end subroutine
end module
