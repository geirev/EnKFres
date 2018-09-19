module m_sm_iniensemble
contains
subroutine sm_iniensemble
use mod_localdefs
use m_printhelp
use m_interface
use m_iniens
implicit none
character(len=1) cvar
character(len=4) cnum
character(len=2) comm
character(len=80) ensfile
character(len=100) command
logical ex
integer i,j,iflag,iopt
integer iens

character(len=80) eclname
character(len=80) ensname
character(len=80) ensdir
character(len=80) ecldir


eclname(:)=' '; i=len_trim(eclbase); eclname(1:i)=trim(eclbase)
ensname(:)=' '; i=len_trim(ensbase); ensname(1:i)=trim(ensbase)
ecldir(:)=' ';  ecldir(1:2)='./'
ensdir(:)=' ';  ensdir(1:2)='./'



comm=' '
do 
   print '(a)','    ======================================================='
   print '(a)','    = Generation of initial ensemble file                 ='
   print '(a)','    ======================================================='
   print '(a)','    =   a: run iniens to generate ensemble file           ='
   print '(a)','    =   b: generate static ensemble file                  ='
   print '(a)','    =   c: compute statistcs of original petrophysics     ='
   print '(a)','    =   h: help                                           ='
   print '(a)','    =   q: return to main menu                            ='
   print '(a)','    ======================================================='
   write(*,'(a)',advance='no')'Command: '
   read(*,*)comm

   select case (trim(comm))
   case('q')
      exit

   case('a')
! default name of initial ensemble file
         ensfile(:)=' '
         i=len_trim(ensbase)
         ensfile(1:i)=trim(ensbase)
         ensfile(i+1:i+8)='F0000.uf'

! remove old initial ensemble file
         i=len_trim(ensfile)
         command(:)=' '
         command(1:6)='rm -f '
         command(6+1:6+i)=trim(ensfile)
         print *,'Executing command: ',trim(command)
         call system(trim(command))

! flag for generation of ensemble file
         iflag=1
         call iniens(trim(ensfile),nrens,iflag)

         inquire(file=trim(ensfile),exist=ex)
         if (ex) then
            i=len_trim(ensfile)
            command(:)=' '
            command(1:3)='cp '
            command(3+1:3+i)=trim(ensfile)
            command(4+i:4+i)=' '
            command(4+i+1:4+i+i)=trim(ensfile)
            command(4+i+i-7:4+i+i-7)='A'
            print *,'Executing command: ',trim(command)
            call system(trim(command))
         endif

   case('b')
         i=len_trim(refdir)
         command(:)=' '
         command(1:3)='cp '
         command(3+1:3+i)=trim(refdir)
         command(3+i+1:3+i+16)='refcaseS0000.uf '
         j=len_trim(ensbase)
         command(3+i+16+1:3+i+16+j)=trim(ensbase)
         command(3+i+16+j+1:3+i+16+j+8)='S0000.uf'
         print *,'Executing command: ',trim(command)
         call system(trim(command))
         iopt=3
         iens=nrens
         cvar='F'  ! dummy here
         cnum(1:4)='0000'

         call interface(ensdir,ensname,ecldir,eclname,cvar,cnum,iens,iopt)

   case('c')
         iflag=3
         call iniens(trim(ensfile),nrens,iflag)

   case('h')
      call printhelp('initialization.txt')

   case default
      print '(a)','Invalid command: try again!'
   end select

enddo

end subroutine
end module
