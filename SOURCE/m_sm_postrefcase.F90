module m_sm_postrefcase
contains
subroutine sm_postrefcase()
use mod_states
use mod_localdefs
use m_mkobsfile
use m_printhelp
use m_getmask
use m_toens
implicit none
character(len=2) comm
character(len=100) command
character(len=80) fileold
character(len=80) filenew
character(len=80) filename
integer i,j
character(len=1) yn
logical ex
type(states) A(1)

comm=' '
do 
   print '(a)','    ======================================================='
   print '(a)','    = Post processing of reference case                   ='
   print '(a)','    ======================================================='
   print '(a)','    =   a: regenerate mask.uf file                        ='
   print '(a)','    =   b: Generate observations.dat file                 ='
   print '(a)','    =   c: Convert restart files to ensemble type files   ='
   print '(a)','    =   h: help                                           ='
   print '(a)','    =   q: return to main menu                            ='
   print '(a)','    ======================================================='
   write(*,'(a)',advance='no')'Command: '
   read(*,*)comm

   select case (trim(comm))
   case('q')
      exit

   case('a')
      call system('rm -f mask.uf')
      call getmask()

   case('b')
      call mkobsfile()

   case('c')
      call toens()

   case('h')
      call printhelp('postprocessing.txt')

   case default
      print '(a)','Invalid command: try again!'
   end select

enddo

end subroutine
end module
