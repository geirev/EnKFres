module m_cnvporo
contains
subroutine cnvporo(oldfile,newfile,variable)
   use mod_dimensions
   implicit none
   character(len=*) oldfile
   character(len=*) newfile
   character(len=*) variable
   integer i

   integer, parameter :: n=nx*ny*nz
   real poro(n)
   real, parameter :: epsilon=1.0E-05

   open(10,file=trim(oldfile))
      read(10,*)poro
   close(10)

   if (trim(variable) == 'PORO') then
      do i=1,n
         poro(i)=max(poro(i),epsilon)
      enddo
   endif

   open(10,file=trim(newfile))
      write(10,'(a)')trim(VARIABLE)
      write(10,'(5(e14.6))')poro(1:n)
      write(10,'(a)')' /'
   close(10)

end subroutine
end module
