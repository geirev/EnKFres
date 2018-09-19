module m_get_obs
contains
subroutine get_obs(obs,nrobs)
   use mod_measurement
   implicit none

   integer,           intent(in)  :: nrobs           ! Number of measurements
   type(measurement), intent(out) :: obs(nrobs)      ! measurements

   integer reclO,j

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Read measurements and store in obs

   inquire(iolength=reclO)obs(1)
   open(10,file='observations.uf',form='unformatted',access='direct',recl=reclO)
   do j=1,nrobs
      read(10,rec=j)obs(j)
      print '(i4,4i3,2(tr1,a8),2f16.6)',j,obs(j)
   enddo
   close(10)

end subroutine get_obs
end module m_get_obs
