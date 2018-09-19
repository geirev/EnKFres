module m_ensmean
contains
subroutine ensmean(A,ave)
   use mod_dimensions
   use mod_states
   implicit none
   type(states), intent(in)  :: A(nrens)
   type(states), intent(out) :: ave
   integer j

   ave=A(1)
   do j=2,nrens
      ave=ave+A(j)
   enddo
   ave=(1.0/real(nrens))*ave

end subroutine ensmean
end module m_ensmean
