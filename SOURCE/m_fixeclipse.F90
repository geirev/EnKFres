module m_fixeclipse
contains
subroutine fixeclipse(A)
! This routine will correct unphysical values which WILL occur following the
! analysis when variables with nongaussian statistics is used.

   use mod_dimensions
   use mod_states
   use m_getmask
   implicit none
   type (states), intent(inout) :: A(nrens)
   real, parameter :: epsilon=1.0E-05
   integer i,j,k,iens
   integer c_pres
   integer c_sg
   integer c_sw
   integer c_so
   integer c_rs
   integer c_rv
   integer c_poro
   integer c_permz
   integer c_permx

   real tmp,so

! the following values needs to be taken from the relperm tables
   real, parameter :: swmin=0.033    
   real, parameter :: swmax=1.000000

   real, parameter :: sgmin=0.000000
   real, parameter :: sgmax=1.000000

   real, parameter :: somin=0.000000
   real, parameter :: somax=0.967   

   real, parameter :: pomin=0.00001 ! 1.00E-05
   real, parameter :: pomax=0.4 ! 1.0  ! 0.4

   real, parameter :: pemin=0.1 ! 1.00E-05
   real, parameter :: pemax=10000.0 ! 100000.0   ! 6500.0

   real, parameter :: prmin=1.0 ! 100.00


   logical :: lfirst=.true.
   integer ifirst  ! used to define the first active grid point to fix.
                   ! Applies only to Omega south since the first 16 layers are
                   ! not updated.   Should really be coded as a region by region
                   ! correction to allow for different SWFN and SGFN in different
                   ! SATNUM regions.

   ifirst=1
#ifdef OMEGASOUTH
   ifirst=sum(mask(1:nx,1:ny,1:16))+ifirst
#endif
   print *,'fixeclipse: ifirst = ',ifirst
   print *,'fixeclipse: nactive= ',nactive



! Ensure that the porosity is always > 1.0E-06 which is the
! threshold where ECLIPSE removes the gridcell.

   print '(a)',' Tresholds used are defined as: '
   print '(a,f10.6,a,f12.6,a)',' Sw in    [',swmin,':',swmax,']'
   print '(a,f10.6,a,f12.6,a)',' Sg in    [',sgmin,':',sgmax,']'
   print '(a,f10.6,a,f12.6,a)',' So in    [',somin,':',somax,']'
   print '(a,f10.6,a,f12.6,a)',' Poro in  [',pomin,':',pomax,']'
   print '(a,f10.6,a,f12.6,a)',' Perm in  [',pemin,':',pemax,']'


   print '(a)','Writing post analysis corrections to fixeclipse.log.'
   open(10,file='fixeclipse.log')

   do iens=1,nrens
      c_pres=0; c_sg=0; c_sw=0; c_so=0; c_rs=0; c_rv=0; c_poro=0; c_permx=0; c_permz=0
      do i=ifirst,nactive
         if (A(iens)%poro(i) < pomin .or. A(iens)%poro(i) > pomax) c_poro=c_poro+1
         A(iens)%poro(i)=max( min(A(iens)%poro(i),pomax)  ,pomin)
 
         if (A(iens)%permx(i) < pemin .or. A(iens)%permx(i) > pemax) c_permx=c_permx+1
         A(iens)%permx(i)=max( min(A(iens)%permx(i),pemax)  ,pemin)
 
         if (A(iens)%permz(i) < pemin .or. A(iens)%permz(i) > pemax) c_permz=c_permz+1
         A(iens)%permz(i)=max( min(A(iens)%permz(i),pemax)  ,pemin)

         if (A(iens)%pres(i) < epsilon) c_pres=c_pres+1
         A(iens)%pres(i)=max( A(iens)%pres(i)          ,prmin)

         if (A(iens)%sg(i) < sgmin .or. A(iens)%sg(i) > sgmax) c_sg=c_sg+1
         A(iens)%sg(i)  =max( min(A(iens)%sg(i)  ,sgmax) ,sgmin)

         if (A(iens)%sw(i) < swmin .or. A(iens)%sw(i) > swmax) c_sw=c_sw+1
         A(iens)%sw(i)  =max( min(A(iens)%sw(i)  ,swmax) ,swmin)

         so=1.0-A(iens)%sw(i)-A(iens)%sg(i)
         if (so < somin-epsilon) then
            c_so=c_so+1
            write(10,'(a,i4,i7,3g13.4)')'somin:',iens,i,so,A(iens)%sw(i),A(iens)%sg(i)

            A(iens)%sw(i)=1.0 - somin - A(iens)%sg(i)

            so=1.0-A(iens)%sw(i)-A(iens)%sg(i)
            write(10,'(a,i4,i7,3g13.4)')'somin:',iens,i,so,A(iens)%sw(i),A(iens)%sg(i)
            write(10,*)
         elseif (so > somax+epsilon) then
            c_so=c_so+1
            write(10,'(a,i4,i7,3g13.4)')'somax:',iens,i,so,A(iens)%sw(i),A(iens)%sg(i)

            A(iens)%sw(i)=1.0 - somax - A(iens)%sg(i)

            so=1.0-A(iens)%sw(i)-A(iens)%sg(i)
            write(10,'(a,i4,i7,3g13.4)')'somax:',iens,i,so,A(iens)%sw(i),A(iens)%sg(i)
            write(10,*)
         endif


!         if (1.0 - (A(iens)%sw(i)+A(iens)%sg(i)) > somax) then
!            c_so=c_so+1
!            tmp=A(iens)%sw(i)+A(iens)%sg(i)
!            A(iens)%sw(i)=max(A(iens)%sw(i)/tmp,swmin)
!            A(iens)%sg(i)=max(A(iens)%sg(i)/tmp,sgmin)
!         endif
 
         if (A(iens)%rs(i) < epsilon) c_rs=c_rs+1
         A(iens)%rs(i)  =max( A(iens)%rs(i)            ,0.0)
 
#ifdef VAPOIL
         if (A(iens)%rv(i) < 0.0) c_rv=c_rv+1
         A(iens)%rv(i)  =max( A(iens)%rv(i)            ,0.0)
#endif
      enddo

      if (c_pres+c_sg+c_sw+c_rs+c_rv+c_poro+c_permx+c_permz > 0) then
         if (lfirst) write(10,'(a11,a3,9a6)')' ens member','   ',&
               'c_pres', '  c_sg', '  c_sw', '  c_so', '  c_rs', '  c_rv', '  c_po', '  c_px', '  c_pz'
         lfirst=.false.
         write(10,'(a,i4,a,9i6)')' iens= ',iens,' : ',c_pres*100/(nactive-ifirst+1),&
                                                c_sg*100/(nactive-ifirst+1),&
                                                c_sw*100/(nactive-ifirst+1),&
                                                c_so*100/(nactive-ifirst+1),&
                                                c_rs*100/(nactive-ifirst+1),&
                                                c_rv*100/(nactive-ifirst+1),&
                                                c_poro*100/(nactive-ifirst+1),&
                                                c_permx*100/(nactive-ifirst+1),&
                                                c_permz*100/(nactive-ifirst+1)
      endif


#ifdef MULTZ
      A(iens)%multz=max(0.0,A(iens)%multz)
#endif
#ifdef MULTFLT
      A(iens)%multflt=max(0.0,A(iens)%multflt)
#endif
   enddo
   close(10)


end subroutine fixeclipse
end module m_fixeclipse


