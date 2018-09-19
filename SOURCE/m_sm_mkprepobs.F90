module m_sm_mkprepobs
contains
subroutine sm_mkprepobs()
use mod_localdefs
implicit none
character(len=250) command
character(len=1) yn
character(len=8), allocatable :: wells(:)
logical ex
integer  ierr,i,j,hist,nrwells
character(len=8) vars(6)
real stddev

!command(:)=' '
!command=trim(scriptdir)//'mkprepobs.sh'
!print '(2a)','Executing command: ',trim(command)
!call system(trim(command))


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Check if old file exists
inquire(file='prepobs.def',exist=ex)
if (ex) then
   write(*,'(a)')'prepobs.def exists:'
   call system('cat prepobs.def')
   write(*,'(a)',advance='no')'Backup current file and create a new default one (y/n): '
   read(*,*)yn
   if (yn == 'n') then
      return
   else
      call system('mv prepobs.def prepobs.old')
      print '(a)','Old prepobs.def copied to prepobs.old'
   endif
endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Read well names from wells.dat
inquire(file='wells.dat',exist=ex)
if (.not.ex) then
   print *,'You first need to run and post process the reference case to generate wells.dat'
   return
else
   open(10,file='wells.dat')
   allocate(wells(1))
   do i=1,1000
      read(10,'(a)',end=100,err=100)wells(1)
   enddo
   100 nrwells=i-1
   rewind(10)
   deallocate(wells)
   allocate(wells(nrwells))
   do i=1,nrwells
      read(10,'(a)')wells(i)
   enddo
   close(10)
endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Chose between production or history data as observations
print '(a)','The data to be assimilated is extracted from the summary files from the reference simulation.'
print '(a)','In twin experiments you are assimilating simulated rates, like WGOR and WWCT etc.'
print '(a)','In real experiments you are assimilating the history rates, like WGORH and WWCTH etc.'
write(*,'(a)',advance='no')'Chose (0) for simulated rates or (1) for history rates: '
read(*,*) hist

vars(1)(:)='WBHP    '
vars(2)(:)='WOPR    '
vars(3)(:)='WGPR    '
vars(4)(:)='WWPR    '
vars(5)(:)='WWCT    '
vars(6)(:)='WGOR    '

if (hist == 1) vars(:)(5:5)='H'

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ierr=10

open(10,file='prepobs.def')
write(10,'(a)')'observations.dat'
write(10,'(a)')'nnnnn'
write(10,'(a)')'Wellname+nr+Keywords+-+Stddev---+---'
do i=1,nrwells
   do j=1,6
      if (vars(j)(2:4) == 'WCT') then
         stddev=0.05
      else
         stddev=0.00
      endif
      write(10,'((a8,tr1,i2.2,tr1,a8,tr1,a1,tr1,e9.3,tr1,i3))')wells(i),i,vars(j),'F',stddev,ierr
   enddo
enddo
close(10)

print '(a)','The new prepobs.def file is:'
call system('cat prepobs.def')

write(*,'(a)')'Warning: if you remove lines this will affect which variables are dumped to history.dat,'
write(*,'(a)')'You need to activate measurements to be assimilated.'
write(*,'(a)',advance='no')'Do you want to edit the file (y/n): '
read(*,*)yn

if (yn == 'y') then
   call system('vi prepobs.def')
endif

end subroutine
end module
