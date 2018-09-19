module m_inimultflt
   use mod_dimensions
   character(len=50) :: faultname(nrflts)
   real faultmean(nrflts),faultstd(nrflts)
contains
subroutine inimultflt(iflag,returnflag)
   use mod_localdefs
   implicit none
   logical, intent(out) :: returnflag
   integer, intent(in)  :: iflag

   integer num,i
   character(len=1) yn
   logical ex,exA,exB
   character(len=1) :: sep=''''
   character(len=1) :: sepp='"'
   character(len=80)fname


   returnflag=.false.
   inquire(file='multflt.txt',exist=exA)
   inquire(file='multflt.dat',exist=exB)

   if (.not.exA .and. .not.exB) then
      print *,'Neither multflt.txt or multflt.dat exists!'
      print '(a)','The simplest is to fill in values in the xls file'
      print '(a)','       petrostatistics.xls'
      print '(5a)','and store the MULTFLT sheet in ',sep,'Text (Tab delimited) (*.txt)',sep,' format as'
      print '(a)','       multflt.txt'
      returnflag=.true.
      return
   endif

   if (exA) then
      write(*,'(a)',advance='no')'Want to covert multflt.txt to multflt.dat (y/n): '
      read(*,*)yn
      if (yn == 'y') then
         open(10,file='multflt.sh')
         write(10,'(a) ',advance='no')'cat multflt.txt | '
         write(10,'(4a)',advance='no')'sed -e',sep,'1,3d',sep
         write(10,'(4a)',advance='no')' -e ',sep,'s/,/./g',sep
         write(10,'(6a)',advance='no')' -e ',sepp,'s/',sep,'//g',sepp
         write(10,'(a)') ' > multflt.dat'
         close(10)
         call system('sh ./multflt.sh')
      endif
   endif

   inquire(file='multflt.dat',exist=exB)
   if (.not.exB) then
      print '(a)','Can not find multflt.dat: returns'
      returnflag=.true.
      return
   endif

   open(10,file='multflt.dat')
      faultname=' '
      do i=1,nrflts
         read(10,*,err=999)num,faultname(i),faultmean(i),faultstd(i)
         print '(i3,a10,2f12.4)',num,trim(faultname(i)),faultmean(i),faultstd(i)
      enddo
   close(10)

   if (iflag == 1) return

! Generating an initial Prior/MULTFLT.INC if this doesn't alrady exist
   fname(:)=' '
   i=len_trim(pridir)
   fname(1:i)=trim(pridir)
   fname(i+1:i+9)='MULTFLT.INC'

   inquire(file=trim(fname),exist=ex)
   yn='n'
   if (ex) then
      write(*,'(3a)',advance='no')'The file ',trim(fname),' exists. Overwrite with new prior (y/n)? '
      read(*,*)yn
   else
      yn='y'
   endif

   if (yn == 'y') then
      fname(i+1:i+11)='MULTFLT.INC'
      print *,'Saves random prior ',trim(fname)
      open(11,file=trim(fname))
         write(11,'(a)')'MULTFLT'
         do i=1,nrflts
            write(11,'(a,1x,a,a,f8.5,2x,a)')sep,trim(faultname(i)),sep,faultmean(i),' /'
         enddo
         write(11,'(a)')'/'
      close(11)
   endif

   return
   999 close(10)
   print *,'Error reading multflt.dat: Check nrfaults and number of faults in multflt.dat'

end subroutine
end module
