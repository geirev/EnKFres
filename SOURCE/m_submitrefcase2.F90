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
integer jobid

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
#ifdef LINUXPGF
command(:)=' '
command(1:4)='cat '
i=len_trim(eclbase)
command(5:5+i)=trim(eclbase)

i1=5+i
i2=5+i+24
command(i1:i2)='_orig.DATA | /lib/cpp -P '

i=len_trim(defini)
if (i>0) then
   i1=i2+1
   i2=i1+i
   command(i1:i2)=trim(defini)
endif

i=len_trim(defres)
if (i>0) then
   i1=i2+1
   i2=i1+i
   command(i1:i2)=trim(defres)
endif

i1=i2+1
i2=i1+3
command(i1:i2)=' > '

i=len_trim(rundir)
i1=i2
i2=i1+i-1
command(i1:i2)=trim(rundir)

i=len_trim(eclbase)
i1=i2+1
i2=i1+i
command(i1:i2)=trim(eclbase)

i1=i2
i2=i1+4
command(i1:i2)='.DATA'

#else
command(:)=' '
command='cat '//trim(eclbase)//'_orig.DATA | /lib/cpp -P '&
       &//trim(defini)//' '//trim(defres)//' '//trim(defref)//' > '//trim(rundir)//trim(eclbase)//'.DATA'
#endif
write(*,*)'Executing command: ',trim(command)
call system(trim(command))



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! SCHEDULE.INC file generation
#ifdef LINUXPGF
command(:)=' '
command(1:36)='cat SCHEDULE_orig.INC | /lib/cpp -P '

i2=len_trim(command)
i=len_trim(defres)
i1=i2+1
i2=i1+i
command(i1:i2)=trim(defini)

i=len_trim(defres)
if (i>0) then
   i1=i2+1
   i2=i1+i
   command(i1:i2)=trim(defres)
endif

i1=i2+1
i2=i1+3
command(i1:i2)=' > '

i=len_trim(rundir)
i1=i2
i2=i1+i-1
command(i1:i2)=trim(rundir)

command(i1+1:i2+12)='SCHEDULE.INC'

#else
command(:)=' '
command='cat SCHEDULE_orig.INC | /lib/cpp -P '//trim(defini)//' '//trim(defres)//' > '//trim(rundir)//'SCHEDULE.INC'
#endif
write(*,*)'Executing command: ',trim(command)
call system(trim(command))


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Adding an END to end of rundir/SCHEDULE.INC file
filename(:)=' '
#ifdef LINUX
i=len_trim(rundir)
filename(1:i)=trim(rundir)
filename(i+1:i+12)='SCHEDULE.INC'
#else
filename=trim(rundir)//'SCHEDULE.INC'
#endif
open(10,file=trim(filename),position='append')
write(10,'(a)')'END'
close(10)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Hydro command for reference case
if (trim(version) == 'hydro') then
   filename(:)=' '
   i=len_trim(rundir)
   filename(1:i)=trim(rundir)
   filename(i+1:i+10)='eclipse.in'
   print '(2a)','Creates: ',trim(filename)
   open(10,file=trim(filename))
   write(10,'(a)')'1'
   write(10,'(a)')trim(eclbase)
   write(10,'(a)')'7'
   write(10,'(a)')'1'
   write(10,'(a)')''
   write(10,'(a)')''
   write(10,'(a)')''
   write(10,'(a)')''
   write(10,'(a)')''
   write(10,'(a)')''
   write(10,'(a)')''
   close(10)

#ifdef LINUX
   command(:)=' '
   command(1:3)='cd '
   i=len_trim(rundir)
   command(3+1:3+i)=trim(rundir)
   command(3+i+1:3+i+2)='; '
   i1=3+i+3
   i2=i1+95
   command(i1:i2)='@eclipse < eclipse.in  2> /dev/null | grep "Job <" | cut -f2 -d"<" | cut -f1 -d">" > jobid.tmp'
#else
   command(:)=' '
   command='cd '//trim(rundir)//'; '//&
          &'@eclipse < eclipse.in  2> /dev/null | grep "Job <" | cut -f2 -d"<" | cut -f1 -d">" > jobid.tmp'
#endif


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Statoil command for reference case
elseif (trim(version) == 'statoil') then
   call system('pwd > pwd.tmp')
   currentdir(:)=' '
   open(10,file='pwd.tmp')
      read(10,'(a)')currentdir
   close(10)
   call system('rm -f pwd.tmp')
   print *,'read current directory as: ',trim(currentdir)
#ifdef LINUX
   command='eclrun -p eclipse -v 2005a_1 -s 150Mb -i '//trim(currentdir)//'/'//trim(rundir)//&
        &'ECLIPSE.DATA -r i686Linux_RH30,mips64 -q normal -n 1  2 > /dev/null | grep "Job <" | cut -f2 -d"<" | cut -f1 -d">" > '&
        &//trim(currentdir)//'/'//trim(rundir)//'/jobid.tmp'
#else
   command='eclrun -p eclipse -v 2005a_1 -s 150Mb -i '//trim(currentdir)//'/'//trim(rundir)//&
        &'ECLIPSE.DATA -r i686Linux_RH30,mips64 -q normal -n 1  2 > /dev/null | grep "Job <" | cut -f2 -d"<" | cut -f1 -d">" > '&
        &//trim(currentdir)//'/'//trim(rundir)//'/jobid.tmp'
#endif


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! rogaland command for reference case
elseif (trim(version) == 'rogaland') then
   filename(:)=' '
   i=len_trim(rundir)
   filename(1:i)=trim(rundir)
   filename(i+1:i+10)='eclipse.in'
   print '(2a)','Creates: ',trim(filename)
   open(10,file=trim(filename))
   write(10,'(a)')'ECLIPSE'
   write(10,'(a)')''
   write(10,'(a)')''
   write(10,'(a)')''
   write(10,'(a)')''
   write(10,'(a)')''
   write(10,'(a)')''
   write(10,'(a)')''
   write(10,'(a)')''
   write(10,'(a)')''
   write(10,'(a)')''
   close(10)

#ifdef LINUX
   command(:)=' '
   command(1:3)='cd '
   i=len_trim(rundir)
   command(3+1:3+i)=trim(rundir)
   command(3+i+1:3+i+2)='; '
   i1=3+i+3
   i2=i1+95
   command(i1:i2)='@eclipse < eclipse.in  2> /dev/null | | tail -1 | cut -c5-8 > jobid.tmp'
#else
   command(:)=' '
   command='cd '//trim(rundir)//'; '//'@eclipse < eclipse.in  2> /dev/null | | tail -1 | cut -c5-8 > jobid.tmp'
#endif

endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   write(*,*)'Executing command: ',trim(command)
   call system(trim(command))

#ifdef LINUX
   filename(:)=' '
   i=len_trim(rundir)
   filename(1:i)=trim(rundir)
   filename(i+1:i+10)='jobid.tmp'
#else
   filename(:)=' '
   filename=trim(currentdir)//'/'//trim(rundir)//'jobid.tmp'
#endif
   print *,'filename: ',trim(filename)
   call system('pwd')
   inquire(file=trim(filename),exist=ex)
   print '(2a,l1)','Reads: ',trim(filename),ex
   if (ex) then
      open(10,file=trim(filename))
      read(10,*)submitrefcase
      close(10)
   else
      print *,'submitrefcase: file ',trim(filename),' does not exist'
      stop
   endif

   print '(a,i8)','Simulation is running as jobid',submitrefcase

end function
end module
