module m_enscov
contains
subroutine enscov(A,ave,cov,nrens)
   use mod_states
   implicit none
   integer, intent(in) :: nrens
   type(states), intent(in)  :: A(nrens)
   type(states), intent(in)  :: ave
   type(states), intent(out) :: cov
   integer i,j,k,iens,i2,j2

   cov=0.0
   i2=nx/2
   j2=ny/2
   do iens=1,nrens
      do k=1,nz
      do j=1,ny
      do i=1,nx
         cov%poro(i,j,k)=cov%poro(i,j,k) &
                        + (A(iens)%poro(i2,j2,k)-ave%poro(i2,j2,k))&
                        * (A(iens)%poro(i,j,k)-ave%poro(i,j,k))
      enddo
      enddo
      enddo
   enddo
   cov%poro=cov%poro/real(nrens-1)

end subroutine enscov
end module m_enscov
