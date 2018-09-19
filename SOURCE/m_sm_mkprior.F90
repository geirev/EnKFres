module m_sm_mkprior
contains
subroutine sm_mkprior()
use mod_states
use mod_localdefs
use m_cnvporo
use m_printhelp
use m_iniens
implicit none
character(len=2) comm
character(len=80) fileold
character(len=80) filenew
character(len=80) filename
integer i,j
character(len=1) yn
logical ex

comm=' '
do 
   print '(a)','    ======================================================='
   print '(a)','    = Actions required for initialization of EnKF Eclipse ='
   print '(a)','    ======================================================='
   print '(a)','    =   a: use original model as prior for reference case ='
   print '(a)','    =   b: make a new random prior for the reference case ='
   print '(a)','    =   h: help                                           ='
   print '(a)','    =   q: return to main menu                            ='
   print '(a)','    ======================================================='
   write(*,'(a)',advance='no')'Command: '
   read(*,*)comm

   select case (trim(comm))
   case('q')
      exit

   case('a')
      fileold(:)=' '
      filenew(:)=' '
      i=len_trim(pridir)
      print *,i
      fileold(1:i)=trim(pridir)
      filenew(1:i)=trim(pridir)


      fileold(i+1:i+9)='PORO.ORIG'
      filenew(i+1:i+8)='PORO.INC'
      print *,'processing file: ',trim(fileold)
      inquire(file=trim(fileold),exist=ex)
      if (ex) then
         call cnvporo(trim(fileold),trim(filenew),'PORO')
      else
         print *,'ERROR: ',trim(fileold),' does not exist and must be created.'
      endif

      fileold(i+1:i+10)='PERMX.ORIG'
      filenew(i+1:i+9)='PERMX.INC'
      print *,'processing file: ',trim(fileold)
      inquire(file=trim(fileold),exist=ex)
      if (ex) then
         call cnvporo(trim(fileold),trim(filenew),'PERMX')
      else
         print *,'ERROR: ',trim(fileold),' does not exist and must be created.'
      endif



      fileold(i+1:i+10)='PERMZ.ORIG'
      filenew(i+1:i+9)='PERMZ.INC'
      print *,'processing file: ',trim(fileold)
      inquire(file=trim(fileold),exist=ex)
      if (ex) then
         call cnvporo(trim(fileold),trim(filenew),'PERMX')
      else
         print *,'ERROR: ',trim(fileold),' does not exist and must be created.'
      endif

      print *,'done and ready to proceed!'
      print *,' '


   case('b')
      print *,'Generation of a random prior for reference case: (normally used in twin experiments).'
      print *,'The random prior is generated using the iniens program which is application dependent.'
      print *,'You will therefore need to edit the program so that the initial ensemble statistics'
      print *,'is consistent with the geomodel for your reservoir.'
      print *
      write(*,'(a)',advance='no')'Proceed with generation of the random prior (y/n): '
      read(*,*)yn

      if (yn == 'y' ) then

         if (lfacies) then
!            if [ -r logmeas.dat ]
!            then
!               cp logmeas.dat $pridir
!            fi
!            if [ -r facies.in ]
!            then
!               cp facies.in $pridir
!            else
!               print you need a facies.in file in Run
!               exit
!            fi
!            rm -f ensemble*.uf PERMX.INC PERMZ.IN PORO.INC
!            vi facies.in; cp facies.in ..
!            print "Ready to submit reference run"
!            iniens.$arch
             print *,'No facies support yet!'
         else
            filename(:)=' '
            call iniens(trim(filename),1,2)
         endif


      endif







      

   case('h')
      call printhelp('poroperm.txt')

   case default
      print '(a)','Invalid command: try again!'
   end select

enddo

end subroutine
end module
