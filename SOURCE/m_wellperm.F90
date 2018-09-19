module m_wellperm
contains
subroutine  wellperm(A,nrens)
   use mod_states
   implicit none
   integer, intent(in) :: nrens
   type(states), intent(in)  :: A(nrens)
   logical ex

   integer, parameter :: iwdim=20
   integer iw(iwdim)
   integer jw(iwdim)
   integer kw(iwdim)
   character(len=3) tag3
   character(len=80) fname
   integer i,j

   open(10,file='iw.in')
      do i=1,iwdim
         read(10,*)iw(i),jw(i),kw(i)
         print *,iw(i),jw(i),kw(i)
      enddo
   close(10)

   fname(:)=' '
   fname(1:8)='iw'//tag3//'.dat'
   inquire(file=trim(fname),exist=ex)
   do j=1,50
      write(tag3,'(i3.3)')j
      open(10,file=trim(fname))
         do i=1,iwdim
            write(10,'(i3,3g12.4)')i,A(j)%poro(iw(i),jw(i),kw(i)),&
                                     A(j)%permx(iw(i),jw(i),kw(i)),&
                                     A(j)%permz(iw(i),jw(i),kw(i))
         enddo
      close(10)
   enddo

end subroutine
end module
