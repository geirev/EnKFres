module m_sm_mkschedule
contains
subroutine sm_mkschedule()
use mod_localdefs
implicit none
character(len=200) record
character(len=80) filename
logical ex
integer i,line,idate


print *
call system('ls -C')
print *

write(*,'(a)',advance='no')'Chose original Eclipse schedule file: '
read(*,*)filename

inquire(file=trim(filename),exist=ex)
if (.not.ex) then
   print '(a)','File not found, returns.'
   return
endif

inquire(file='SCHEDULE_orig.INC',exist=ex)
if (ex) then
   print '(a)','Old SCHEDULE_orig.INC moved to SCHEDULE_orig.OLD'
   call system('mv SCHEDULE_orig.INC SCHEDULE_orig.OLD')
endif


open(10,file=trim(filename))
open(11,file='SCHEDULE_orig.INC')
line=3
idate=0
do
   record(:)=' '
   read(10,'(a)',end=100,err=100)record
   write(11,'(a)')trim(record)

   if (record(1:5)=='DATES')  then
      line=0
   else
      line=line+1
   endif

   if (line == 2) then
      idate=idate+1
      write(11,'(a,i4.4)')'--END',idate
      write(11,'(a)')'--==============================================================='
      write(11,'(a)')''
   endif

enddo

100 close(10)
close(11)


print '(a)','==> New SCHEDULE_orig.INC file is generated'
print '(a)','==> It may be wise to diff it with the original file'
print *




!command(:)=' '
!i=len_trim(scriptdir)
!command(1:i)=trim(scriptdir)
!command(i+1:i+13)='mkschedule.sh'
!print *,'+++',trim(command),'+++'
!
!inquire(file=trim(command),exist=ex)
!if (ex) then
!   call system(trim(command))
!else
!   print *,'Cannot find the mkschedule script file.'
!   print *,'No action taken.'
!endif

end subroutine
end module
