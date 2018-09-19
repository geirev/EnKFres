module m_wellstat
contains
subroutine wellstat(ave,var,obs,nrobs)
   use mod_states
   use mod_measurement
   implicit none
   type(states), intent(in) :: ave
   type(states), intent(in) :: var
   integer, intent(in) :: nrobs
   type(measurement), intent(in) :: obs(nrobs)
   type(measurement) tmp
   integer i,iw

   tmp%i=0;  tmp%j=0; tmp%k=0

   print *,'wellstat: Predicted well statistiscs:'
   do i=1,nrobs
      iw=obs(i)%iw
      tmp%iw=iw
      tmp%wgname=obs(i)%wgname
      tmp%keywrd=obs(i)%keywrd

      select case (trim(obs(i)%keywrd))
      case('WBHP','WBHPH')
         tmp%d  =ave%BHP(iw);  tmp%var=sqrt(var%BHP(iw))
         print '(i4,4i3,2(tr1,a8),2f16.6)',i,tmp
      case('WOPR','WOPRH')
         tmp%d  =ave%OPR(iw);  tmp%var=sqrt(var%OPR(iw))
         print '(i4,4i3,2(tr1,a8),2f16.6)',i,tmp
      case('WGPR','WGPRH')
         tmp%d  =ave%GPR(iw);  tmp%var=sqrt(var%GPR(iw))
         print '(i4,4i3,2(tr1,a8),2f16.6)',i,tmp
      case('WWPR','WWPRH')
         tmp%d  =ave%WPR(iw);  tmp%var=sqrt(var%WPR(iw))
         print '(i4,4i3,2(tr1,a8),2f16.6)',i,tmp
      case('WWCT','WWCTH')
         tmp%d  =ave%WCT(iw);  tmp%var=sqrt(var%WCT(iw))
         print '(i4,4i3,2(tr1,a8),2f16.6)',i,tmp
      case('WGOR','WGORH')
         tmp%d  =ave%GOR(iw);  tmp%var=sqrt(var%GOR(iw))
         print '(i4,4i3,2(tr1,a8),2f16.6)',i,tmp
      end select
   enddo

end subroutine wellstat
end module m_wellstat
