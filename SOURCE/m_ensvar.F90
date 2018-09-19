module m_ensvar
contains
subroutine ensvar(A,ave,var)
   use mod_dimensions
   use mod_states
   implicit none
   type(states), intent(in)  :: A(nrens)
   type(states), intent(in)  :: ave
   type(states), intent(out) :: var
   integer j

   var=0.0
   do j=1,nrens
      var=var+(A(j)-ave)*(A(j)-ave)
   enddo
   var=(1.0/real(nrens-1))*var

end subroutine ensvar
end module m_ensvar
