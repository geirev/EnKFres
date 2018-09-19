module m_sm_mkincludes
contains
subroutine sm_mkincludes()
use mod_states
use mod_localdefs
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
   print '(a)','    = Updating of include files for EnKF program          ='
   print '(a)','    ======================================================='
   print '(a)','    =   a: Create files with eclipse variables.           ='
   print '(a)','    =   b: Update include files.                          ='
   print '(a)','    =   c: Print number of active gridcells               ='
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

      command(i+1:i+12)='mkeclfsm.sh '
      j=len_trim(refdir)
      command(i+12+1:i+12+j)=trim(refdir)

      print '(2a)','Excecuting command: ',trim(command)
      call system(trim(command))
      print '(a)','Variables in summary header file: '
      call system('cat eclfsm.vars')

      command(i+1:i+12)='mkeclsum.sh '
      print '(2a)','Excecuting command: ',trim(command)
      call system(trim(command))
      print '(a)','Variables in summary files: '
      call system('cat eclsum.vars')

      command(i+1:i+12)='mkeclres.sh '
      print '(2a)','Excecuting command: ',trim(command)
      call system(trim(command))
      print '(a)','Variables in restart file: '
      call system('cat eclres.vars')

   case('b')
      command(:)=' '
      i=len_trim(scriptdir)
      command(1:i)=trim(scriptdir)

      j=len_trim(codedir)
      command(i+18+1:i+18+j)=trim(codedir)

      command(i+1:i+18)='eclresincfiles.sh '
      print '(2a)','Executing command: ',trim(command)
      call system(trim(command))

      command(i+1:i+18)='eclfsmincfiles.sh '
      print '(2a)','Executing command: ',trim(command)
      call system(trim(command))


      command(i+1:i+18)='eclsumincfiles.sh '
      print '(2a)','Executing command: ',trim(command)
      call system(trim(command))


   case('c')
      filename(:)=' '
      filename=trim(refdir)//trim(eclbase)//'.F0000'

      open(10,file=trim(filename))
         read(10,*)
         read(10,*)
         read(10,'(t61,i12)')iactive
      close(10)
      print *,'Please set nactive in mod_dimensions.F90 to: ',iactive

      print *,'You should recompile the code before proceeding!'

   case('h')
      call printhelp('includefiles.txt')

   case default
      print '(a)','Invalid command: try again!'
   end select

enddo

end subroutine
end module
