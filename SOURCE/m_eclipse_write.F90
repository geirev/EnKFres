module m_eclipse_write
contains

integer function write_integer(fieldname,n,fieldtype,field)
      implicit none
      character(len=8), intent(in) :: fieldname
      integer, intent(in) :: n
      character(len=4), intent(in) :: fieldtype
      character(len=1) :: sep=''''
      integer, intent(in) :: field(n)
      integer i,i1,i2,m,j

! Writing header
      write(10,'(t2,a1,a8,a1,t13,i11,t25,a1,a4,a1)')sep,fieldname,sep,n,sep,fieldtype,sep
      if (n > 0) then

! Writing blocks of 1000 elements
         do i=1,int(real(n)/1000.0)+1
            i1=(i-1)*1000+1
            i2=min(i*1000,n)

! Writing 6 elements at each line followed by "new record"
            m=0
            do j=i1,i2
               write(10,'(I12)',advance='no')field(j)
               m=m+1
               if (mod(m,6)==0 .or. j==i2 .or. j==n) then
                  write(10,'(/)',advance='no')
                  m=0
               endif
            enddo

         enddo
      endif
      write_integer=0
end function write_integer

integer function write_logical(fieldname,n,fieldtype,field)
      implicit none
      character(len=8), intent(in) :: fieldname
      integer, intent(in) :: n
      character(len=4), intent(in) :: fieldtype
      character(len=1) :: sep=''''
      logical, intent(in) :: field(n)
      integer i,i1,i2,j,m

! Writing header
      write(10,'(t2,a1,a8,a1,t13,i11,t25,a1,a4,a1)')sep,fieldname,sep,n,sep,fieldtype,sep
      if (n > 0) then

! Writing blocks of 1000 elements
         do i=1,int(real(n)/1000.0)+1
            i1=(i-1)*1000+1
            i2=min(i*1000,n)

! Writing 25 elements at each line followed by "new record"
            m=0
            do j=i1,i2
               write(10,'(tr2,l1)',advance='no')field(j)
               m=m+1
               if (mod(m,25)==0 .or. j==i2 .or. j==n) then
                  write(10,'(/)',advance='no')
                  m=0
               endif
            enddo

         enddo
      endif
      write_logical=0
end function write_logical

integer function write_double(fieldname,n,fieldtype,field)
      implicit none
      character(len=8), intent(in) :: fieldname
      integer, intent(in) :: n
      character(len=4), intent(in) :: fieldtype
      character(len=1) :: sep=''''
      double precision,    intent(in) :: field(n)
      integer i,i1,i2,j,m
      character(len=23) string

! Writing header
      write(10,'(t2,a1,a8,a1,t13,i11,t25,a1,a4,a1)')sep,fieldname,sep,n,sep,fieldtype,sep
      if (n > 0) then

! Writing blocks of 1000 elements
         do i=1,int(real(n)/1000.0)+1
            i1=(i-1)*1000+1
            i2=min(i*1000,n)

! Writing 3 elements at each line followed by "new record"
            m=0
            do j=i1,i2

! Changing E to D
               write(string,'(d23.14)')field(j)
               string(20:20)='D'
               write(10,'(a23)',advance='no')string
               m=m+1
               if (mod(m,3)==0 .or. j==i2 .or. j==n) then
                  write(10,'(/)',advance='no')
                  m=0
               endif
            enddo
         enddo
      endif
      write_double=0
end function write_double

integer function write_real(fieldname,n,fieldtype,field)
      implicit none
      character(len=8), intent(in) :: fieldname
      integer, intent(in) :: n
      character(len=4), intent(in) :: fieldtype
      character(len=1) :: sep=''''
      real*4,  intent(in) :: field(n)
      integer i,i1,i2,m,j

! Writing header
      write(10,'(t2,a1,a8,a1,t13,i11,t25,a1,a4,a1)')sep,fieldname,sep,n,sep,fieldtype,sep
      if (n > 0) then

! Writing blocks of 1000 elements
         do i=1,int(real(n)/1000.0)+1
            i1=(i-1)*1000+1
            i2=min(i*1000,n)

! Writing 4 elements at each line followed by "new record"
            m=0
            do j=i1,i2
               write(10,'(e17.8)',advance='no')field(j)
               m=m+1
               if (mod(m,4)==0 .or. j==i2 .or. j==n) then
                  write(10,'(/)',advance='no')
                  m=0
               endif
            enddo
         enddo
      endif
      write_real=0
end function write_real

integer function write_char(fieldname,n,fieldtype,field)
      implicit none
      character(len=8), intent(in) :: fieldname
      integer, intent(in) :: n
      character(len=4), intent(in) :: fieldtype
      character(len=1) :: sep=''''
      character(len=10),  intent(in) :: field(n)
      integer i,i1,i2,m,j
      write(10,'(t2,a1,a8,a1,t13,i11,t25,a1,a4,a1)')sep,fieldname,sep,n,sep,fieldtype,sep
      if (n > 0) then
! Writing blocks of 1000 elements
         do i=1,int(real(n)/1000.0)+1
            i1=(i-1)*1000+1
            i2=min(i*1000,n)
! Writing 7 elements at each line followed by "new record"
            m=0
            do j=i1,i2
               write(10,'(tr1,a10)',advance='no')field(j)
               m=m+1
               if (mod(m,7)==0 .or. j==i2 .or. j==n) then
                  write(10,'(/)',advance='no')
                  m=0
               endif
            enddo
         enddo
      endif
      write_char=0
end function write_char

end module m_eclipse_write
