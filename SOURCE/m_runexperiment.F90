module m_runexperiment
contains
subroutine runexperiment(iens1,iens2)
use mod_states
use mod_localdefs
!use mod_ensemble
use m_submitjob
use m_interface
use m_prepobs
use m_ensstat
use m_enkf
use m_random
use m_preprft
implicit none
integer, intent(inout) :: iens1
integer, intent(inout) :: iens2
character(len=2) comm
character(len=200) command
character(len=80) filename
character(len=200) record
character(len=100) dummy
character(len=100) obsfile
integer i,j,len
character(len=1) yn
character(len=1) qstat
logical ex
!type(states) A(1)
character(len=6) cjobid

logical :: verbose=.false.
logical :: pert_record=.false.


integer num              ! counter for data times
character(len=4) cnum
integer next             ! next data time to integrate to
character(len=4) cnext
integer past             ! past data time for cleaning of files
character(len=4) cpast
integer iens             ! counter for ensemble members
character(len=4) cens
character(len=80) ensdir
character(len=80) ecldir
character(len=1) cvar
character(len=4) corat,cwrat,cgrat
real orat,wrat,grat
integer iopt
character(len=200) jobid(nrens)

logical done(nrens)
logical errmem(nrens)
character(len=3) status(nrens)
integer nrpen,nrrun,nrdon,nrerr,nrtot,nrexi
integer ipen,issu,irun,idon,ierr,itot,iexi
integer if
integer iflag

real tt(3)
   
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Preparing all files etc
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   write(corat,'(f4.2)')orat_pert
   write(cwrat,'(f4.2)')wrat_pert
   write(cgrat,'(f4.2)')grat_pert

! Removing old observations.uf file
   call system('rm -f observations.uf')

! Checking if prepobs.def exists
   inquire(file='prepobs.def',exist=ex)
   if (.not.ex) then
      if (.not.noana .and. .not.spinup .and. .not.subexit) then
         print '(a)','You need to generate prepobs.def first'
         return
      endif
   endif



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Loop over data times
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   do num=initime,fintime-1
      write(cnum,'(i4.4)')num


! Next data time
      if (spinup) then
         next=fintime
      else
         next=num+1
      endif
      write(cnext,'(i4.4)')next

      print '(/,4a)','Integrating ensemble from ',cnum,' to ',cnext


! Cleaning and compressing older files
      if (num > 3) then
         past=num-3
         write(cpast,'(i4.4)')past
         command(:)=' '; command='rm -f '//trim(ensbase)//'F'//cpast//'.uf'
         call system(trim(command))
      endif

      if (num > 2) then
         inquire(file=trim(ensbase)//'A'//cpast//'.uf',exist=ex)
         if (ex) then
            command(:)=' '; command='gzip -f '//trim(ensbase)//'A'//cpast//'.uf&'
            call system(trim(command))
         endif

         inquire(file=trim(ensbase)//'S'//cpast//'.uf',exist=ex)
         if (ex) then
            command(:)=' '; command='gzip -f '//trim(ensbase)//'S'//cpast//'.uf&'
            call system(trim(command))
         endif
      endif

! defines for ECLIPSE.DATA and SCHEDULE.INC files
      defini(:)=' '
      defres(:)=' '
      if (num==0) then
         defini='-DDEFINI'
      else
         defini(:)=' '
         defres='-DDEFRESTART'
      endif

! prepare ECLIPSE.DATA file
     command(:)=' '
     command='cat '//trim(eclbase)//'_orig.DATA | /lib/cpp -P '&
            &//trim(defini)//trim(defres)//' > '//trim(eclbase)//'.DATA'
     if (verbose) write(*,*)'Executing command: ',trim(command)
     call system(trim(command))


! prepare SCHEDULE.INC file
      command(:)=' '
      command='cat SCHEDULE_orig.INC | /lib/cpp -P '//trim(defini)//trim(defres)//&
             &' | sed -e "s/--END'//cnext//'/END/" > '//'SCHEDULE.INC'
      if (verbose) write(*,*)'Executing command: ',trim(command)
      call system(trim(command))

! prepare RESTART.INC file
      open(10,file='RESTART.INC')
         write(10,'(a)')'RESTART'
         write(10,'(3a,i4,a)')'''',trim(ECLBASE),'''',num,' /'
      close(10)




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Submit ensemble integration (for each data time)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (skipensint) then
         print '(a)','Ensemble integration skipped due to true e-flag.'
         skipensint=.false.

         inquire(file=trim(ensbase)//'F'//cnext//'.uf',exist=ex)
         if (.not.ex) then
            print '(5a)','Restart file ',trim(ensbase),'F',cnext,'.uf does not exist'
            print '(a)','Wrong start time or did you forget to run ensemble integration?'
            print '(a)','returning...'
            return
         endif

! Redo forecast ensemble statistics
         print '(/,a)','Computing forecast ensemble statistics.'
         filename(:)=' '
         filename=trim(ensbase)//'F'//cnext//'.uf'
         open(10,file='ensstat.files')
            write(10,'(a)')trim(filename)
         close(10)
         iflag=0
         call ensstat(iflag)

      else 

         inquire(file=trim(ensbase)//'A'//cnum//'.uf',exist=ex)
         if (.not.ex) then
            print '(5a)','Restart file ',trim(ensbase),'A',cnum,'.uf does not exist'
            print '(a)','Wrong start time or did you forget to compute analysis?'
            print '(a)','returning...'
            return
         endif

         write(*,'(a)')'Submitting members:'
         do iens=iens1,iens2
            write(cens,'(i4.4)')iens

            ecldir(:)=' '
            ecldir='tmpdir_'//cens//'/'

            ensdir(:)=' '
            ensdir='./'

            cvar='A'

            iopt=2

            inquire(file=trim(ecldir),exist=ex)
            if (ex) then
               command(:)=' '; command='rm -rf '//trim(ecldir)
               call system(trim(command))
            endif

            command(:)=' '; command='mkdir '//trim(ecldir)
            call system(trim(command))

            call interface(ensdir, ensbase, ecldir, eclbase, cvar, cnum, iens, iopt)

            if (pert_rates) then
               open(10,file='SCHEDULE.INC')
               open(11,file=trim(ecldir)//'SCHEDULE.INC')
               do 
                  pert_record=.false.

                  record(:)=' '
                  read(10,'(a)',end=100,err=100)record
                  len=len_trim(record)

                  do i=1,len-3
                     if (record(i:i+3) == 'RESV' .or.&
                         record(i:i+3) == 'ORAT' .or.&
                         record(i:i+3) == 'WRAT' .or.&
                         record(i:i+3) == 'GRAT' .or.&
                         record(i:i+3) == 'LRAT') then
                        pert_record=.true.
                        read(record(i+5:len),*)orat,wrat,grat
                        call random(tt,3)
                        orat=orat*(1.0+orat_pert*tt(1))
                        wrat=wrat*(1.0+wrat_pert*tt(2))
                        grat=grat*(1.0+grat_pert*tt(3))
                        exit
                     endif
                  enddo

                  if (pert_record) then
                     if (grat > 999999) then
                        write(11,'(a,f10.3,1x,f10.3,1x,f11.3,1x,a)')record(1:i+5),orat,wrat,grat,record(i+40:len)
                     else
                        write(11,'(a,f10.3,1x,f10.3,1x,f10.3,1x,a)')record(1:i+5),orat,wrat,grat,record(i+39:len)
                     endif
                  else
                     write(11,'(a)')trim(record)
                  endif

               enddo

               100 close(10)
               close(11)
            else
               command(:)=' '
               command='cp SCHEDULE.INC '//trim(ecldir)
               call system(trim(command))
            endif




            command(:)=' '
            command='cp ECLIPSE.DATA '//trim(ecldir)
            call system(trim(command))

            command(:)=' '
            command='cp RESTART.INC '//trim(ecldir)
            call system(trim(command))

!            command(:)=' '
!            command='cp eclipse.in '//trim(ecldir)
!            call system(trim(command))

            do i=1,3
               jobid(iens)=submitjob(iens,ecldir)

               if (jobid(iens)(1:6) == 'failed') then
                  if (i<3) then
                     print *,'Error submitting member: ',iens,': trying again!'
                  else
                     print *,'Could not get member ',iens,' submitted: exiting'
                     stop
                  endif
               else
                  exit
               endif
            enddo




            
            if (iens < 10) then
               write(*,'(2x,a)',advance='no')cens(4:4)
            elseif (iens < 100) then
               write(*,'(1x,a)',advance='no')cens(3:4)
            else
               write(*,'(a,1x)',advance='no')cens(2:4)
            endif
            call flush(101)
            if (mod(iens,25)==0 .or. iens==iens2) print *,' '

         enddo

         if (subexit) stop 'Ensemble submitted---exiting program'
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Monitor ensemble integrations (for each data time)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         nrpen=0  ! number of pending jobs
         nrrun=0  ! number of running jobs
         nrdon=0  ! number of completed jobs
         nrerr=0  ! number of jobs in error
         nrtot=0  ! sum of nrdon and nrerr
          
         done(:)=.false.
         errmem(:)=.false.
         itot=0
         idon=0
         iexi=0
         ierr=0
         if=1
         do
            call system('sleep 5')
            ipen=0; irun=0 ; issu=0
            do iens=iens1,iens2
               write(cens,'(i4.4)')iens

               if (.not.done(iens)) then

!LSF code
!write(cjobid,'(i6)')jobid(iens)
!command(:)=' '
!command='bjobs -a | grep "'//cjobid//'" |  cut -c17-19 > status'
!call system(trim(command))
!open(10,file='status')
!  read(10,'(a)')status(iens)
!close(10)



! TORQUE code ---------------------------------------------------------------------

                  command(:)=' '
                  command='qstat '//trim(jobid(iens))//' > status'
                  if (verbose) write(*,*)'Executing command: ',trim(command)
                  call system(trim(command))
                  open(10,file='status')
                     read(10,'(a)')qstat
                     read(10,'(a)')qstat
                     read(10,'(t69,a1)')qstat
                  close(10)

                  if (verbose) write(*,*)'status: ',trim(jobid(iens)),'=',qstat
                  select case (qstat)
                  case ('C')    ! C-completed
                     status(iens)='DON'
                  case ('E')    ! E-exiting
                     status(iens)='RUN'
                  case ('H')    ! H-held
                     status(iens)='PEN'
                  case ('Q')    ! Queued
                     status(iens)='PEN'
                  case ('R')    ! Running
                     status(iens)='RUN'
                  case ('T')    ! T-job is being moved?
                     status(iens)='PEN'
                  case ('W')    ! W-job is waiting
                     status(iens)='PEN'
                  case ('S')    ! S-job is suspended
                     status(iens)='SSU'
                  case default
                     print *,'Error in qstat section: status is: ',qstat
                  end select
! TORQUE code ends ---------------------------------------------------------------------


                  select case (status(iens))
                  case('DON')
                     done(iens)=.true.
                     itot=itot+1

                     inquire(file='tmpdir_'//cens//'/'//trim(eclbase)//'.F'//cnext,exist=ex)
                     if (ex) then
                        idon=idon+1
                        write(cens,'(i4.4)')iens
                        ecldir(:)=' '
                        ecldir='tmpdir_'//cens//'/'
                        ensdir(:)=' '
                        ensdir='./'
                        cvar='F'
                        iopt=1
                        call interface(ensdir, ensbase, ecldir, eclbase, cvar, cnext, iens, iopt)
! TESTING INTERFACE
!                        print *,'runexperiment: TESTING INTERFACE ',iens
                        command(:)=' '
                        command(:)='cp '//trim(ecldir)//'/'//trim(eclbase)//'.F'//cnext//' '//trim(ecldir)//'/'//'TESTF'//cnext
!                        print *,'command:',trim(command)
                        call system(trim(command))
                        command(:)=' '
                        command(:)='rm '//trim(ecldir)//'/'//trim(eclbase)//'.F'//cnext
!                        print *,'command:',trim(command)
                        call system(trim(command))
                        iopt=2
                        call interface(ensdir, ensbase, ecldir, eclbase, cvar, cnext, iens, iopt)
                        command(:)=' '
                        command(:)='diff -q '//trim(ecldir)//'/'//trim(eclbase)//'.F'//cnext//' '//trim(ecldir)//'/'//'TESTF'//cnext
!                        print *,'command:',trim(command)
                        call system(trim(command))
! END TESTING INTERFACE
                     else
                        ierr=ierr+1
                        errmem(iens)=.true.
                        print '(a,i3)','Error integrating member ',iens
                     endif

                  case('RUN')
                     irun=irun+1

                  case('PEN')
                     ipen=ipen+1

                  case('SSU')
                     issu=issu+1

                  case('EXI')
                     ierr=ierr+1
                     iexi=iexi+1
                     itot=itot+1
                     errmem(iens)=.true.
                     print '(a,i3)','Exit when integrating member ',iens

                  case default
                     print '(3a,i3)','ERROR: unknown status: ',status(iens),' for member ',iens
                     ierr=ierr+1
                     errmem(iens)=.true.
                     done(iens)=.true.
                     itot=itot+1
                  end select

               endif
            enddo
            if (if==1) then
               print '(a)',    '     PEN RUN SSU ERR EXI TOT DON'
               if=0
            endif
            print '(a,7i4)','    ',ipen,irun,issu,ierr,iexi,itot,idon
            if (itot==iens2-iens1+1) exit
         enddo

         if (ierr > 0) then
            print *,'Stopping simulation due to error in member(s): '
            do iens=iens1,iens2
               if (errmem(iens)) write(*,'(i4)',advance='no')iens
            enddo
            write(*,*)' '
            print '(a)','Check respective tmpdir_???? directories for error messages.'
            stop
         else
            print *,'Integrations completed.'
         endif

! Special case when only a subset of members are integrated.
         if (iens1 /= 1 .or. iens2 /= nrens) then
            write(*,'(a)',advance='no')'All members done and ready to proceed (y/n): '
            read(*,*)yn
            if (yn == 'n') then
               return
            endif
            iens1=1
            iens2=nrens
         endif

! Forecast ensemble statistics
!         print '(/,a)','DO NOT Compute forecast ensemble statistics.'
         print '(/,a)','Computing forecast ensemble statistics.'
         filename(:)=' '
         filename=trim(ensbase)//'F'//cnext//'.uf'
         open(10,file='ensstat.files')
            write(10,'(a)')trim(filename)
         close(10)
         iflag=0
         call ensstat(iflag)

      endif ! skipensint


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Starting analysis computaton
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Creating observations.uf file
      call prepobs(cnext)
!!!!!      call preprft(cnext)

      inquire(file='observations.uf',exist=ex)
      if (ex .and. .not.noana) then
         print '(/,a)','Calling EnKF analysis.'
         obsfile(:)=' '
         obsfile='observations.uf'
         call enkf(ensbase,cnext,trim(obsfile),lenkfana,logperm,.false.)
      else
         print '(/,a,l1)','No observation.uf file or noana set to true: ',noana
         print '(a)','No analysis computed.'
         command(:)=' '
         command='cp '//trim(ensbase)//'F'//cnext//'.uf '//trim(ensbase)//'A'//cnext//'.uf'
         call system(trim(command))
      endif

! Analysis ensemble statistics
      print '(/,a)','Computing analysis ensemble statistics.'
      filename(:)=' '
      filename=trim(ensbase)//'A'//cnext//'.uf'
      open(10,file='ensstat.files')
         write(10,'(a)')trim(filename)
      close(10)
      iflag=1
      call ensstat(iflag)


      if (spinup) exit
   enddo ! data times







end subroutine
end module
