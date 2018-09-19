module m_innovation
contains
subroutine innovation(obs,mem,var,nrobs,aaa)
   use mod_states
   use mod_measurement
   use m_meas_functional
   implicit none

   integer,           intent(in) :: nrobs         ! Number of measurements
   type(measurement), intent(in) :: obs(nrobs)    ! measurements
   type(states),      intent(in) :: mem           ! Ensemble average
   type(states),      intent(in) :: var           ! Ensemble variance
   character(len=1),  intent(in) :: aaa
   real meas(nrobs)
   real innov(nrobs)
   real std(nrobs)

   integer m
   logical lnew

   lnew=.true.
   do m=1,nrobs
      meas(m)= meas_functional(obs(m),mem,lnew)
      innov(m) = obs(m)%d - meas(m)
      lnew=.false.
   enddo

   lnew=.true.
   do m=1,nrobs
      std(m) = sqrt(meas_functional(obs(m),var,lnew))
      lnew=.false.
   enddo

   open(10,file='innovation'//aaa//'.dat')
      do m=1,nrobs
         write(10,'(i6,4g13.5)')m, innov(m), obs(m)%d, meas(m), std(m)
      enddo
   close(10)

   do m=1,nrobs
      print '(a,i3,tr1,3a,3(f13.5,a),f13.5)', &
        ' Innovation ',m,obs(m)%wgname,obs(m)%keywrd,':',innov(m),' (',obs(m)%d,' - ',meas(m),' )',std(m)
   enddo

end subroutine innovation
end module m_innovation

