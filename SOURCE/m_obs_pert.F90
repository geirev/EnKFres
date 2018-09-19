module m_obs_pert
contains
subroutine obs_pert(E,obs,nrens,nrobs)
   use mod_measurement
   use m_random
   implicit none
   integer, intent(in)           :: nrobs
   integer, intent(in)           :: nrens
   type(measurement), intent(in) :: obs(nrobs)
   real, intent(out)             :: E(nrobs,nrens)

   real EE(nrobs,nrens)

   real average(nrobs), variance(nrobs)

   integer iens,m
   


! Create an E matrix with random normal distributed numbers
   call random(E,nrobs*nrens)   

   average=0.0
   do iens=1,nrens
      do m=1,nrobs
         average(m)=average(m)+E(m,iens)
      enddo
   enddo
   average=average/real(nrens)

   variance=0.
   do iens=1,nrens
      do m=1,nrobs
         E(m,iens)=E(m,iens)-average(m)
         variance(m) = variance(m) + E(m,iens)**2.
      enddo
   enddo
   do m=1,nrobs
      variance(m)=variance(m)/real(nrens)
      E(m,1:nrens)=E(m,1:nrens)/sqrt(variance(m))
   enddo

! Introduce correct covariances
   do iens=1,nrens
   do m=1,nrobs
      E(m,iens)=sqrt(obs(m)%var)*E(m,iens)
   enddo
   enddo


end subroutine obs_pert
end module m_obs_pert
