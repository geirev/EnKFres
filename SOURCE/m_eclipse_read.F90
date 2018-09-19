module m_eclipse_read
contains

integer function read_integer(n,field,fieldtype)
      integer, intent(in)  :: n
      integer, intent(out) :: field(n)
      character(len=4), intent(in) :: fieldtype
      integer i
      if (n < 1) then
         !print *,'eclipse_read: reading empty field'
         return
      endif
      if (fieldtype /= 'INTE') then
         close(10)
         print *,'read_integer: attempting to read ',fieldtype,' as integer'
         open(unit=10,file='INTERFACE.ERROR')
            write(10,'(a)')'interface (read_integer) problem'
            write(10,'(a,a,a)')'attempting to read ',fieldtype,' as integer'
            write(10,'(a)')'Serious error likely caused by wrong specification in calling routine'
         close(10)
         stop
      endif
      i=0
      do
         i=i+1
         read(10,'(I12)',advance='NO',EOR=99)field(i)
         if (i == n) exit
         cycle
         99 i=i-1
      enddo 
      read(10,*)
      !print '(6I12)',(field(i),i=1,min(n,50))
      read_integer=0
end function read_integer

integer function read_logical(n,field,fieldtype)
      integer, intent(in)  :: n
      logical, intent(out) :: field(n)
      character(len=4), intent(in) :: fieldtype
      integer i
      if (n < 1) then
         !print *,'eclipse_read: reading empty field'
         return
      endif
      if (fieldtype /= 'LOGI') then
         close(10)
         print *,'read_logical: attempting to read ',fieldtype,' as logical'
         open(unit=10,file='INTERFACE.ERROR')
            write(10,'(a)')'interface (read_logical) problem'
            write(10,'(a,a,a)')'attempting to read ',fieldtype,' as logical'
            write(10,'(a)')'Serious error likely caused by wrong specification in calling routine'
         close(10)
         stop
      endif
      i=0
      do
         i=i+1
         if (mod(i,25) == 0 ) then
            read(10,'(tr2,l1)')field(i)
         else
            read(10,'(tr2,l1)',advance='NO',EOR=99)field(i)
         endif
         if (i == n) exit
         cycle
         99 i=i-1
      enddo 
      read(10,*)
      !print '(25l3)',(field(i),i=1,min(n,50))
      read_logical=0
end function read_logical

integer function read_double(n,field,fieldtype)
      integer, intent(in)  :: n
      double precision,    intent(out) :: field(n)
      character(len=4), intent(in) :: fieldtype
      integer i
      if (n < 1) then
         !print *,'eclipse_read: reading empty field'
         return
      endif
      if (fieldtype /= 'DOUB') then
         close(10)
         print *,'read_double: attempting to read ',fieldtype,' as double'
         open(unit=10,file='INTERFACE.ERROR')
            write(10,'(a)')'interface (read_double) problem'
            write(10,'(a,a,a)')'attempting to read ',fieldtype,' as double'
            write(10,'(a)')'Serious error likely caused by wrong specification in calling routine'
         close(10)
         stop
      endif
      i=0
      do
         i=i+1
         read(10,'(d23.14)',advance='NO',EOR=99)field(i)
         if (i == n) exit
         cycle
         99 i=i-1
      enddo 
      read(10,*)
      !print '(3(d23.14))',(field(i),i=1,min(n,50))
      read_double=0
end function read_double

integer function read_real(n,field,fieldtype)
      integer, intent(in)  :: n
      real*4,  intent(out) :: field(n)
      character(len=4), intent(in) :: fieldtype
      integer i
      if (n < 1) then
         !print *,'eclipse_read: reading empty field'
         return
      endif
      if (fieldtype /= 'REAL') then
         close(10)
         print *,'read_real: attempting to read ',fieldtype,' as real'
         open(unit=10,file='INTERFACE.ERROR')
            write(10,'(a)')'interface (read_real) problem'
            write(10,'(a,a,a)')'attempting to read ',fieldtype,' as real'
            write(10,'(a)')'Serious error likely caused by wrong specification in calling routine'
         close(10)
         stop
      endif
      i=0
      do
         i=i+1
         read(10,'(e17.8)',advance='NO',EOR=99)field(i)
         if (i == n) exit
         cycle
         99 i=i-1
      enddo 
      read(10,*)
      !print '(4(e17.8))',(field(i),i=1,min(n,50))
      read_real=0
end function read_real

integer function read_char(n,field,fieldtype)
      integer, intent(in)  :: n
      character(len=10),  intent(out) :: field(n)
      character(len=4), intent(in) :: fieldtype
      integer i
      if (n < 1) then
         !print *,'eclipse_read: reading empty field'
         return
      endif
      if (fieldtype /= 'CHAR') then
         close(10)
         print *,'read_char: attempting to read ',fieldtype,' as character'
         open(unit=10,file='INTERFACE.ERROR')
            write(10,'(a)')'interface (read_char) problem'
            write(10,'(a,a,a)')'attempting to read ',fieldtype,' as character'
            write(10,'(a)')'Serious error likely caused by wrong specification in calling routine'
         close(10)
         stop
      endif
      i=0
      do
         i=i+1
         read(10,'(tr1,a10)',advance='NO',EOR=99)field(i)
         if (i == n) exit
         cycle
         99 i=i-1
      enddo 
      read(10,*)
      !print '(4(tr1,a10))',(field(i),i=1,min(n,50))
      read_char=0
end function read_char

integer function read_perm(n,field)
      integer, intent(in)  :: n
      real*4,  intent(out) :: field(n)
      integer i
      i=0
      do
         i=i+1
         read(10,'(e14.6)',advance='NO',EOR=99)field(i)
         if (i == n) exit
         cycle
         99 i=i-1
      enddo 
!      print '(5(e14.6))',(field(i),i=1,min(n,50))
      read_perm=0
end function read_perm

end module m_eclipse_read
