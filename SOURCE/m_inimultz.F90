module m_inimultz
   use mod_dimensions
   real multzmean(1:nz-1)
   real multzstd(1:nz-1)
contains
subroutine inimultz(iflag,returnflag)
   use mod_localdefs
   implicit none
   logical, intent(out) :: returnflag
   integer, intent(in)  :: iflag

   integer, parameter :: en=1
   logical ex,exA,exB
   character(len=1) :: sep=''''
   character(len=1) :: sepp='"'
   integer i,num,k
   character(len=1) yn
   character(len=80)fname

   returnflag=.false.
   inquire(file='multz.txt',exist=exA)
   inquire(file='multz.dat',exist=exB)


   if (.not.exA .and. .not.exB) then
      print *,'Neither multz.txt or multz.dat exists!'
      print '(a)','The simplest is to fill in values in the xls file'
      print '(a)','       petrostatistics.xls'
      print '(5a)','and store the MULTZ sheet in ',sep,'Text (Tab delimited) (*.txt)',sep,' format as'
      print '(a)','       multz.txt'
      returnflag=.true.
      return
   endif

   if (exA) then
      write(*,'(a)',advance='no')'Do you want to convert multz.txt to multz.dat (y/n): '
      read(*,*)yn
      if (yn == 'y') then
         open(10,file='multz.sh')
         write(10,'(a) ',advance='no')'cat multz.txt | '
         write(10,'(4a)',advance='no')'sed -e',sep,'1,3d',sep
         write(10,'(4a)',advance='no')' -e ',sep,'s/,/./g',sep
         write(10,'(a)') ' > multz.dat'
         close(10)
         call system('sh ./multz.sh')
      endif
   endif

   inquire(file='multz.dat',exist=exB)
   if (.not.exB) then
      print '(a)','Can not find multz.dat: returns'
      returnflag=.true.
      return
   endif

   open(10,file='multz.dat')
      do i=1,nz-1
         read(10,*,err=998)num,multzmean(i),multzstd(i)
         print '(i3,2f12.4)',num,multzmean(i),multzstd(i)
      enddo
   close(10)


   if (iflag == 1) return

! Generating an initial Prior/MULTZ.INC if this doesn't alrady exist
   fname(:)=' '
   i=len_trim(pridir)
   fname(1:i)=trim(pridir)
   fname(i+1:i+9)='MULTZ.INC'

   inquire(file=trim(fname),exist=ex)
   yn='n'
   if (ex) then
      write(*,'(3a)',advance='no')'The file ',trim(fname),' exists. Overwrite with new prior (y/n)? '
      read(*,*)yn
   else
      yn='y'
   endif

   if (yn == 'y') then
      open(10,file=trim(fname))
         do k=1,nz-1
            write(10,'(a)')'BOX'
            write(10,'(6i5,a)')en, nx, en, ny, k, k,' /'
            write(10,'(a)')'MULTZ'
            select case (nx*ny)
            case(10:99)
               write(10,'(i2,a,E11.5,a)')nx*ny,'*',multzmean(k),' /'
            case(100:999)
               write(10,'(i3,a,E11.5,a)')nx*ny,'*',multzmean(k),' /'
            case(1000:9999)
               write(10,'(i4,a,E11.5,a)')nx*ny,'*',multzmean(k),' /'
            case(10000:99999)
               write(10,'(i5,a,E11.5,a)')nx*ny,'*',multzmean(k),' /'
            case(100000:999999)
               write(10,'(i6,a,E11.5,a)')nx*ny,'*',multzmean(k),' /'
            end select
            write(10,'(a)')'ENDBOX'
            write(10,'(a)')' '
         enddo
      close(10)
   endif

   return
   998 close(10)
   print *,'Error reading multz.dat: Check nz-1 and number of layers in model'


end subroutine
end module
