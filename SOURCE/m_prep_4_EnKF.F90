module m_prep_4_EnKF

! Prepares the input matrices to the EnKF analysis scheme.

contains
subroutine prep_4_EnKF(obs,A,nrobs,E,D,S,innov,R)
   use mod_dimensions
   use mod_states
   use mod_measurement
   use m_obs_pert
   use m_meas_functional
   implicit none

! Input variables
   integer,           intent(in) :: nrobs         ! Number of measurements
   type(states),      intent(in) :: A(nrens)      ! Ensemble of model states 
   type(measurement), intent(in) :: obs(nrobs)    ! measurements

! Output variables
   real, intent(out) :: E(nrobs,nrens)
   real, intent(out) :: D(nrobs,nrens)
   real, intent(out) :: S(nrobs,nrens)
   real, intent(out) :: innov(nrobs)
   real, intent(out) :: R(nrobs,nrobs)

! Local variables 
   real  meanS(nrobs)    ! Automatic array
   integer m,iens
   logical lnew


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Observe ensemble to construct the matrix S=HA
   do iens =1, nrens
      lnew=.true.
      do m =1, nrobs
         S(m,iens)      =  meas_functional(obs(m),A(iens),lnew)
         lnew=.false.
      enddo
   enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Construct observation perturbations E
   call obs_pert(E,obs,nrens,nrobs)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Construct observation error covariance matrix
   R=0.0
   do m=1,nrobs
      R(m,m)=obs(m)%var
   enddo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Construct ensemble of measurements D=d+E
   do iens=1,nrens
      do m=1,nrobs
         D(m,iens)=obs(m)%d+E(m,iens)
      enddo
   enddo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Compute innovation D`=D-HA
   D=D-S

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Compute mean(HA) 
   meanS=0.0
   do iens=1,nrens
   do m=1,nrobs
      meanS(m)=meanS(m)+S(m,iens)
   enddo
   enddo
   meanS=(1.0/real(nrens))*meanS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Compute innovation
   do m=1,nrobs
      innov(m)=obs(m)%d-meanS(m)
   enddo


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Compute HA`=HA-mean(HA)
   do iens=1,nrens
      S(:,iens)=S(:,iens)-meanS(:)
   enddo 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end subroutine prep_4_EnKF
end module m_prep_4_EnKF

