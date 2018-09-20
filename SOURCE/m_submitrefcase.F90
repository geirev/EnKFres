module m_submitrefcase
contains
integer function submitrefcase(prid,rundir)
use mod_states
use mod_localdefs
implicit none
character(len=*), intent(in)  :: rundir   ! directory to run simulation
character(len=*), intent(in)  :: prid   ! directory to find priors


character(len=250) command
character(len=80) filename
character(len=80) defref
character(len=100) currentdir
integer i,j,i1,i2
logical ex
logical verbose
integer jobid
integer*4 istat
character(len=1) yn

inquire(file=trim(pridir)//'PORO.INC',exist=ex)
if (ex) then
    command(:)=' '; command='cp '//trim(pridir)//'PORO.INC '//trim(refdir)
   write(*,*)'Executing command: ',trim(command)
   call system(trim(command))
else
   print '(a)','WARNING: you need to create priors, PORO.INC'
   return
endif

inquire(file=trim(pridir)//'PERMX.INC',exist=ex)
if (ex) then
    command(:)=' '; command='cp '//trim(pridir)//'PERMX.INC '//trim(refdir)
   write(*,*)'Executing command: ',trim(command)
   call system(trim(command))
else
   print '(a)','WARNING: you need to create priors, PERMX.INC'
   return
endif

inquire(file=trim(pridir)//'PERMZ.INC',exist=ex)
if (ex) then
    command(:)=' '; command='cp '//trim(pridir)//'PERMZ.INC '//trim(refdir)
   write(*,*)'Executing command: ',trim(command)
   call system(trim(command))
else
   print '(a)','WARNING: you need to create priors, PERMZ.INC'
   return
endif

#ifdef MULTZ
inquire(file=trim(pridir)//'MULTZ.INC',exist=ex)
if (ex) then
   command(:)=' '; command='cp '//trim(pridir)//'MULTZ.INC '//trim(refdir)
   write(*,*)'Executing command: ',trim(command)
   call system(trim(command))
else
   print *,'ERROR: ',trim(pridir),'MULTZ.INC does not exist and must be created.'
   return
endif
#endif

#ifdef MULTFLT
inquire(file=trim(pridir)//'MULTFLT.INC',exist=ex)
if (ex) then
   command(:)=' '; command='cp '//trim(pridir)//'MULTFLT.INC '//trim(refdir)
   write(*,*)'Executing command: ',trim(command)
   call system(trim(command))
else
   print *,'ERROR: ',trim(pridir),'MULTFLT.INC does not exist and must be created.'
   return
endif
#endif

#ifdef EQUIL
inquire(file=trim(pridir)//'EQUIL.INC',exist=ex)
if (ex) then
   command(:)=' '; command='cp '//trim(pridir)//'EQUIL.INC '//trim(refdir)
   write(*,*)'Executing command: ',trim(command)
   call system(trim(command))
else
   print *,'ERROR: ',trim(pridir),'EQUIL.INC does not exist and must be created.'
   return
endif
#endif


defini(:)=' '
defres(:)=' '
defref(:)=' '
if (initime==0) then
   defini='-DDEFINI'
   defref='-DREFCASE'
else
   defini='        ' 
   defres='-DDEFRESTART'
endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ECLIPSE.DATA file generation
command(:)=' '
command='cat '//trim(eclbase)//'_orig.DATA | /lib/cpp -P '&
       &//trim(defini)//' '//trim(defres)//' '//trim(defref)//' > '//trim(rundir)//trim(eclbase)//'.DATA'
write(*,*)'Executing command: ',trim(command)
call system(trim(command))



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! SCHEDULE.INC file generation
command(:)=' '
command='cat SCHEDULE_orig.INC | /lib/cpp -P '//trim(defini)//' '//trim(defres)//' > '//trim(rundir)//'SCHEDULE.INC'
write(*,*)'Executing command: ',trim(command)
call system(trim(command))


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Adding an END to end of rundir/SCHEDULE.INC file
filename(:)=' '
filename=trim(rundir)//'SCHEDULE.INC'
write(*,*)'Adding END to: ',trim(filename)
open(10,file=trim(filename),position='append')
write(10,'(a)')'END'
close(10)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Hydro command for reference case
!if (trim(version) == 'hydro') then
!   filename(:)=' '
!   i=len_trim(rundir)
!   filename(1:i)=trim(rundir)
!   filename(i+1:i+10)='eclipse.in'
!   print '(2a)','Creates: ',trim(filename)
!   open(10,file=trim(filename))
!   write(10,'(a)')'1'
!   write(10,'(a)')trim(eclbase)
!   write(10,'(a)')'7'
!   write(10,'(a)')'1'
!   write(10,'(a)')''
!   write(10,'(a)')''
!   write(10,'(a)')''
!   write(10,'(a)')''
!   write(10,'(a)')''
!   write(10,'(a)')''
!   write(10,'(a)')''
!   close(10)
!
!   command(:)=' '
!   command='cd '//trim(rundir)//'; '//&
!          &'@eclipse < eclipse.in  2> /dev/null | grep "Job <" | cut -f2 -d"<" | cut -f1 -d">" > jobid.tmp'
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Statoil command for reference case
!elseif (trim(version) == 'statoil') then
!   call system('pwd > pwd.tmp')
!   currentdir(:)=' '
!   open(10,file='pwd.tmp')
!      read(10,'(a)')currentdir
!   close(10)
!   call system('rm -f pwd.tmp')
!   print *,'read current directory as: ',trim(currentdir)
!   command='eclrun -p eclipse -v 2005a_1 -s 150Mb -i '//trim(currentdir)//'/'//trim(rundir)//&
!        &'ECLIPSE.DATA -r i686Linux_RH30,mips64 -q normal -n 1  2 > /dev/null | grep "Job <" | cut -f2 -d"<" | cut -f1 -d">" > '&
!        &//trim(currentdir)//'/'//trim(rundir)//'/jobid.tmp'
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! rogaland command for reference case
!if (trim(version) == 'rogalandold') then
!   filename(:)=' '
!   i=len_trim(rundir)
!   filename(1:i)=trim(rundir)
!   filename(i+1:i+10)='eclipse.in'
!   print '(2a)','Creates: ',trim(filename)
!   open(10,file=trim(filename))
!   write(10,'(a)')'2010.2'
!   write(10,'(a)')'ECLIPSE'
!   write(10,'(a)')''
!   write(10,'(a)')'b'
!   write(10,'(a)')'m'
!   close(10)
!
!   command(:)=' '
!   command='cd '//trim(rundir)//'; '//'@eclipse < eclipse.in  2> /dev/null | tail -1 | cut -c5-10 > jobid.tmp'
!
!
!
if (trim(version) == 'rogaland') then
   filename(:)=' '
   filename='submit.sh'

   inquire(file='submit.sh',exist=ex)
   if (.not.ex) then
      print *,'Generating new submit.sh file'
      open(10,file='submit.sh')
         write(10,'(a)')'#!/bin/sh'
         write(10,'(a)')'#PBS -k o'
         write(10,'(a)')'#PBS -l nodes=1,walltime=00:02:00'
         write(10,'(a)')'#PBS -N job.tmp'
         write(10,'(a)')'#PBS -j oe'
         write(10,'(a)')'#PBS -q batch'
!         write(10,'(3a)')'python2.7 /home/geve/Dropbox/ERT/ERTshared/bin/run_eclipse.py 2010.2 '//trim(eclbase)//' 1'
         write(10,'(3a)')trim(eclrundir)//'run_eclipse.py 2010.2 '//trim(eclbase)
      close(10)
      write(*,'(a)',advance='no')'Do you want to edit the file submit.sh (y/n): '
      read(*,*)yn
      if (yn == 'y') then
         call system('vi submit.sh')
      endif
   endif

   command(:)=' '
   command='cp submit.sh '//trim(rundir)
   if (verbose) write(*,*)'Executing command: ',trim(command)
   call system(trim(command))

   istat=getcwd(currentdir); if (istat.ne.0) stop 'error in cwd'
   print *,'Read current directory as: ',trim(currentdir)

   command(:)=' '
   command='qsub -d "'//trim(currentdir)//'/'//trim(rundir)//'" submit.sh > '//trim(rundir)//'jobid.tmp'

endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   write(*,*)'Executing command: ',trim(command)
   call system(trim(command))

   filename(:)=' '
   filename=trim(currentdir)//'/'//trim(rundir)//'jobid.tmp'
   print *,'filename: ',trim(filename)
   inquire(file=trim(filename),exist=ex)
   print '(2a,a,l1)',' Reads: ',trim(filename),' ',ex
   if (ex) then
      command(:)=' '
      command='cat '//trim(filename)//' | cut -f1 -d. > ijob.tmp'
      call system(trim(command))
      open(10,file='ijob.tmp')
      read(10,'(i10)')submitrefcase
      close(10)

   else
      print *,' submitrefcase: file ',trim(filename),' does not exist'
      stop
   endif

   print '(a,i10)','Simulation is running as jobid:',submitrefcase

end function
end module
