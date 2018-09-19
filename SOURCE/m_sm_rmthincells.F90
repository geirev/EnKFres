module m_sm_rmthincells
contains
subroutine sm_rmthincells()
use mod_states
use mod_localdefs
use m_sm_referencecase
use m_printhelp
implicit none
character(len=2) comm
character(len=100) command
character(len=80) filename
integer i,j
character(len=1) yn
logical ex
type(states) A(1)
integer iactive

comm=' '
do 
   print '(a)','    ======================================================='
   print '(a)','    = Procedure for removing thin cells in model          ='
   print '(a)','    ======================================================='
   print '(a)','    =   a: Prepare for removal of thin cells              ='
   print '(a)','    =   b: Run refcase to generate grid file              ='
   print '(a)','    =   c: Run eclpost to create  remove.mod              ='
   print '(a)','    =   h: help                                           ='
   print '(a)','    =   q: return to main menu                            ='
   print '(a)','    ======================================================='
   write(*,'(a)',advance='no')'Command: '
   read(*,*)comm

   select case (trim(comm))
   case('q')
      exit

   case('a')
      command(:)=' '
      i=len_trim(scriptdir)
      command(1:i)=trim(scriptdir)

      command(i+1:i+15)='rmthincells1.sh'
      print '(2a)','Excecuting command: ',trim(command)
      call system(trim(command))

   case('b')
      call sm_referencecase()

   case('c')
      command(:)=' '
      i=len_trim(scriptdir)
      command(1:i)=trim(scriptdir)

      command(i+1:i+15)='rmthincells2.sh'
      print '(2a)','Excecuting command: ',trim(command)
      call system(trim(command))

   case('h')
      call printhelp('rmthincells.txt')

   case default
      print '(a)','Invalid command: try again!'
   end select

enddo

end subroutine
end module
