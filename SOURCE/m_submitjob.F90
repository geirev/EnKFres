module m_submitjob
contains
character(len=200) function submitjob(iens,ecldir)
use mod_states
use mod_localdefs
implicit none
character(len=80), intent(in)  :: ecldir   ! directory to run simulation
integer, intent(in) :: iens
logical :: verbose=.false.


character(len=1) yn
character(len=250) command
character(len=80) filename
character(len=100) currentdir
integer i,j,i1,i2
logical ex
integer jobid
integer*4 istat

character(len=3) cens

istat=getcwd(currentdir); if (istat.ne.0) stop 'error in cwd'
if (verbose) print *,'read current directory as: ',trim(currentdir)

   command(:)=' '
if (trim(version) == 'hydro') then
!   command='cd '//trim(ecldir)//' > /dev/null; '//&
!          &'@eclipse < eclipse.in  2> /dev/null | grep "Job <" | cut -f2 -d"<" | cut -f1 -d">" > jobid.tmp'
elseif (trim(version) == 'statoil') then
!   command='eclrun -p eclipse -v 2004a_1 -s 150Mb -i '//trim(currentdir)//'/'//trim(ecldir)//&
!          &'ECLIPSE.DATA -r i686Linux -q normal -n 1  2> /dev/null | grep "Job <" | cut -f2 -d"<" | cut -f1 -d">" > jobid.tmp'
elseif (trim(version) == 'rogaland') then
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
         write(10,'(3a)')trim(eclrundir)//'run_eclipse.py 2010.2 '//trim(eclbase)
      close(10)
      write(*,'(a)',advance='no')'Do you want to edit the file submit.sh (y/n): '
      read(*,*)yn
      if (yn == 'y') then
         call system('vi submit.sh')
      endif
   endif

   command(:)=' '
   command='cp submit.sh '//trim(ecldir)
   if (verbose) write(*,*)'Executing command: ',trim(command)
   call system(trim(command))

   command(:)=' '
   command='qsub -d "'//trim(currentdir)//'/'//trim(ecldir)//'" submit.sh > '//trim(ecldir)//'jobid.tmp'

endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   if (verbose)   write(*,*)'Executing command: ',trim(command)
   call system(trim(command))
!   write(*,*)'Executing command: ',trim(command)

   filename(:)=' '
   filename=trim(ecldir)//'jobid.tmp'
   submitjob=' '
   open(10,file=trim(filename))
      read(10,*,err=100)submitjob
   close(10)
   if (verbose)   write(*,*)'submitjob=',trim(submitjob)
   
   return

   100 submitjob='failed'


end function
end module
