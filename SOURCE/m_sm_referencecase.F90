module m_sm_referencecase
contains
subroutine sm_referencecase()
use mod_states
use mod_localdefs
use m_submitrefcase
use m_getmask
use m_printhelp
implicit none
character(len=2) comm
character(len=100) command
character(len=80) filename
integer i,j
character(len=1) yn
logical ex
type(states) A(1)
integer jobid
character(len=6) cjobid

comm=' '
do 
   print '(a)','    ======================================================='
   print '(a)','    = Actions required for initialization of EnKF Eclipse ='
   print '(a)','    ======================================================='
   print '(a)','    =   a: submit reference simulation                    ='
   print '(a)','    =   b: check job status                               ='
   print '(a)','    =   h: help                                           ='
   print '(a)','    =   q: return to main menu                            ='
   print '(a)','    ======================================================='
   write(*,'(a)',advance='no')'Command: '
   read(*,*)comm

   select case (trim(comm))
   case('q')
      exit

   case('a')

      inquire(file=trim(refdir),exist=ex)
      if (.not.ex) call system('mkdir Refcase')

      jobid=submitrefcase(trim(pridir),trim(refdir))

   case('b')
      print *,'Xjobid=',jobid
      write(cjobid,'(i6)')jobid
      command(:)=' '
      command='qstat '//cjobid
      print '(2a)','Executing command: ',trim(command)
      call system(trim(command))

   case('h')
      call printhelp('referencecase.txt')

   case default
      print '(a)','Invalid command: try again!'
   end select

enddo

end subroutine
end module
