module m_iniequil
   use mod_dimensions
   real eqdepth(eqldims), eqp(eqldims),      eqwoc(eqldims),   eqpc1(eqldims)
   real eqgoc(eqldims),   eqpc2(eqldims),    eqrs(eqldims),    eqrv(eqldims)
   real eqacc(eqldims),   eqwocstd(eqldims), eqgocstd(eqldims)
contains
subroutine iniequil(iflag,returnflag)
   use mod_localdefs
   implicit none
   logical, intent(out) :: returnflag
   integer, intent(in)  :: iflag

   logical ex,exA,exB
   character(len=1) :: sep=''''
   character(len=1) :: sepp='"'
   integer i,num
   character(len=1) yn
   character(len=80)fname


   returnflag=.false.

   inquire(file='equil.txt',exist=exA)
   inquire(file='equil.dat',exist=exB)

   if (.not.exA .and. .not.exB) then
      print *,'Neither equil.txt or equil.dat exists!'
      print '(a)','The simplest is to fill in values in the xls file'
      print '(a)','       petrostatistics.xls'
      print '(5a)','and store the EQUIL sheet in ',sep,'Text (Tab delimited) (*.txt)',sep,' format as'
      print '(a)','       equil.txt'
      returnflag=.true.
      return
   endif

   if (exA) then
      write(*,'(a)',advance='no')'Want to covert equil.txt to equil.dat (y/n): '
      read(*,*)yn
      if (yn == 'y') then
         open(10,file='equil.sh')
         write(10,'(a) ',advance='no')'cat equil.txt | '
         write(10,'(4a)',advance='no')'sed -e',sep,'1,2d',sep
         write(10,'(4a)',advance='no')' -e ',sep,'s/,/./g',sep
         write(10,'(a)') ' > equil.dat'
         close(10)
         call system('sh ./equil.sh')
      endif
   endif

   inquire(file='equil.dat',exist=exB)
   if (.not.exB) then
      print '(a)','Can not find equil.dat: returns'
      returnflag=.true.
      return
   endif

   open(10,file='equil.dat')
      do i=1,eqldims
         read(10,*,err=997)eqdepth(i), eqp(i), eqwoc(i), eqpc1(i), eqgoc(i),&
                           eqpc2(i), eqrs(i), eqrv(i), eqacc(i), eqwocstd(i), eqgocstd(i)
         print '(11f12.4)',eqdepth(i), eqp(i), eqwoc(i), eqpc1(i), eqgoc(i),&
                           eqpc2(i), eqrs(i), eqrv(i), eqacc(i), eqwocstd(i), eqgocstd(i)
      enddo
   close(10)


   if (iflag == 1) return

! Generating an initial Prior/MULTZ.INC if this doesnt already exist
   fname(:)=' '
   i=len_trim(pridir)
   fname(1:i)=trim(pridir)
   fname(i+1:i+9)='EQUIL.INC'

   inquire(file=trim(fname),exist=ex)
   yn='n'
   if (ex) then
      write(*,'(3a)',advance='no')'The file ',trim(fname),' exists. Overwrite with new prior (y/n)? '
      read(*,*)yn
   else
      yn='y'
   endif

   if (yn == 'y') then
      print *,'Saves prior ',trim(fname)
      open(10,file=trim(fname))
         write(10,'(a)')'EQUIL'
         write(10,'(a)')'-- depth       P     WOC      Pc     GOC      Pc  Rs(d) Rv(d) Acc(IIP)'
         do i=1,eqldims
            write(10,'(6f8.2,3i5,a)')eqdepth(i), eqp(i), eqwoc(i), eqpc1(i), eqgoc(i),&
                                     eqpc2(i), nint(eqrs(i)), nint(eqrv(i)), nint(eqacc(i)),' /'
         enddo
      close(10)
   endif


   return
   997 close(10)
   print *,'Error reading equil.dat: Check eqldims and number of eql-regions'

end subroutine
end module
