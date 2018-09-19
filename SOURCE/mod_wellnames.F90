module mod_wellnames
character(len=8), public, allocatable :: wells(:)
integer nrwells
contains
subroutine wellnames()
implicit none
integer i
   if (allocated(wells))  return

   open(10,file='wells.dat')
      allocate(wells(1))
      i=1
      do
         read(10,'(a)',end=96)wells(1)
         i=i+1
         cycle
         96 exit
      enddo
      nrwells=i-1
      deallocate(wells)

      allocate(wells(nrwells))
      rewind(10)
      do i=1,nrwells
         read(10,'(a)')wells(i)
         print '(a,i4,tr2,a)','Well ',i,wells(i)
      enddo
   close(10)

end subroutine
end module
