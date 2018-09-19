module m_sm_initialization
contains
subroutine sm_initialization()
use mod_localdefs
use m_printhelp
character(len=2) comm
character(len=100) command
integer i

comm=' '
do 
   print '(a)','    ======================================================='
   print '(a)','    = Actions required for initialization of EnKF Eclipse ='
   print '(a)','    ======================================================='
   print '(a)','    =   a: Customize Eclipse data file                    ='
   print '(a)','    =   b: Compilation of code                            ='
   print '(a)','    =   q: return to main menu                            ='
   print '(a)','    ======================================================='
   write(*,'(a)',advance='no')'Command: '
   read(*,*)comm

   select case (trim(comm))
   case('q')
      exit

   case('a')
      call printhelp('eclipse_data.txt')

   case('b')
      call printhelp('compilation2.txt')

   case default
      print '(a)','Invalid command: try again!'
   end select

enddo

end subroutine
end module
