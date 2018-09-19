Module m_enkf
contains
subroutine enkf(ensbase,cnum,obsfile,ana,logperm,lporoperm)
! Computes the EnKF analysis
   use mod_dimensions
   use mod_states
   use mod_measurement

   use m_ensmean            ! Computes ensemble mean
   use m_ensvar             ! Computes ensemble variance

   use m_get_nrens          ! get number of ensemble members
   use m_get_ensemble       ! load model forecast ensemble

   use m_get_nrobs          ! get number of observations
   use m_get_obs            ! load observations

   use m_ensio

   use m_innovation
   use m_fixeclipse
   use m_wellstat

   use m_prep_4_EnKF
   use m_set_random_seed2
   use m_tecfld
   implicit none

   character(len=80),  intent(in) :: ensbase
   character(len=4),   intent(in) :: cnum
   character(len=80),  intent(in) :: obsfile
   character(len=2),   intent(in) :: ana
   logical,            intent(in) :: logperm
   logical,            intent(in) :: lporoperm

   
! Ensemble matrix
   type(states), allocatable      :: A(:)      ! Ensemble matrix
   type(states4) mem4                          ! Real 4 member 
   type(states)                   :: ave,var   ! Statistics
!   integer nrens                               ! Size of ensemble

! Observations
   type(measurement), allocatable :: obs(:)    ! Measurements
   integer nrobs                               ! Number of measurements

! Variables used for the EnKF analysis
   real, allocatable, dimension(:,:) :: S,E,D,R
   real, allocatable, dimension(:)   :: scaling,innov
   real, allocatable, dimension(:,:,:) :: poroens


   integer reclA,iflg,len

   real truncation
   integer mode_analysis
   logical update_randrot

! Work arrays used for plotting
   character(len=30) dateinfo
   real, allocatable,dimension(:,:) :: OLD, NEW, DIFF,RESIDUAL
   real, allocatable,dimension(:)   :: DIFF2


! Local variables in main program
   real scale
   integer iens,i,j,m,k
   real varDiFF2, aveDiFF2, Dummy
   character(len=9) rident  
   logical ex
   logical :: verbose=.true.

! Consistency checks...


   logical :: checkobs=.true.
   logical :: lfirst=.true.
   real, allocatable :: hphstd(:),obsstd(:)
   real, parameter :: alpha=1.5
   real stdx

! Given random seed
!   integer seedsze
!   integer, allocatable ::  putseed(:)

   integer :: ntest=199
   integer :: itest=15
   integer :: jtest=25
   logical :: lscale=.true.


   character(len=120) command
   character(len=80) ensFfile
   character(len=80) ensAfile
   integer global_ndim

   real, allocatable :: pop(:,:)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   print *,'----------------------------------------------------------'
   print *,'EnKF: starts......'


   ensFfile=' '
   ensAfile=' '
   len=len_trim(ensbase)
   ensFfile(1:len)=trim(ensbase)
   ensAfile(1:len)=trim(ensbase)
   ensFfile(len+1:len+1)='F'
   ensAfile(len+1:len+1)='A'
   ensFfile(len+2:len+5)=cnum(1:4)
   ensAfile(len+2:len+5)=cnum(1:4)
   ensFfile(len+6:len+8)='.uf'
   ensAfile(len+6:len+8)='.uf'
   print *,trim(ensFfile)
   print *,trim(ensAfile)


! Dimension of basic state variable
   global_ndim = 7*nactive+10*nw
#ifdef WELLSTATE
   global_ndim = global_ndim+nXGRP+nRSEG+nXWEL+nSCON+nXCON
#endif
#ifdef VAPOIL
   global_ndim = global_ndim+nactive
#endif
#ifdef GAUSS2
   global_ndim = global_ndim+2*nx*ny*nz
#endif
#ifdef MULTZ
   global_ndim = global_ndim+nz-1
#endif
#ifdef MULTFLT
   global_ndim = global_ndim+nrflts
#endif
#ifdef EQUIL
   global_ndim = global_ndim+2*eqldims
#endif

   inquire(iolength=reclA)mem4
   if (reclA/4 /= global_ndim) then
      print *,'EnKF: global_ndim is not correctly specified',global_ndim,reclA/4
      stop
   endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Set a variable random seed
   print *,'----------------------------------------------------------'
   print *,'EnKF: set random seed'
   call set_random_seed2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Read measurements and store in d
   print *,'----------------------------------------------------------'
   write(*,'(1x,a)',advance='no')'EnKF: Counting measurements to: '
   nrobs=get_nrobs()
   print *,nrobs

   print *,'EnKF: Reading measurements'
   allocate(obs(nrobs))
   call get_obs(obs,nrobs)


   if (lporoperm .and. nrobs == 0)then
      print *,'EnKF: No data found. Returns.'
      return
   endif
      
   if (nrobs == 0) then
      print *,'EnKF: No data found. Continues with ensemble integration.'
      command=' '
      len=len_trim(ensFfile)
      command(1:6)='cp -f '
      command(7:7+len)=trim(ensFfile)
      command(7+len+1:7+len+1)=' '
      command(7+len+2:7+len+2+len)=trim(ensAfile)
      print *,'Executing: ',trim(command)
      call system(trim(command))
      return
   endif


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Read ensemble and store in A
   print *,'----------------------------------------------------------'
!   print *,'EnKF: Reading forecast ensemble'
!   nrens=get_nrens(trim(ensFfile))
   print *,'EnKF: number of ensemble members is',nrens
   allocate(A(nrens))
   call get_ensemble(trim(ensFfile),A)

   if (logperm) then
      print *,'----------------------------------------------------------'
      print *,'EnKF: Logperm is active'
      do iens=1,nrens
         if (iens==1) print *,'maxperm: ',iens,maxval(A(iens)%permx),maxloc(A(iens)%permx)
         A(iens)%permx=log(1.0+A(iens)%permx)
         A(iens)%permz=log(1.0+A(iens)%permz)
         if (iens==1) print *,'maxperm: ',iens,maxval(A(iens)%permx),maxloc(A(iens)%permx)
      enddo
   endif


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Consistency checks on forecast ensemble
   print *,'----------------------------------------------------------'
   print *,'EnKF: Ensemble well statistics'

   call ensmean(A,ave)
   call ensvar(A,ave,var)

   call wellstat(ave,var,obs,nrobs)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   Construct observation perturbations        ==> return in E
!   Construct ensemble of measurements D=d+E
!   Observe ensemble to construct the matrix HA
!   Compute innovation D`=D-HA                 ==> return in D
!   Compute mean(HA) 
!   Compute HA`=HA-mean(HA)                    ==> return in S
   print *,'----------------------------------------------------------'
   print *,'EnKF: Compute input matrices (E, D, S) for the analysis'

   allocate(E(nrobs,nrens))
   allocate(D(nrobs,nrens))
   allocate(S(nrobs,nrens))
   allocate(scaling(nrobs))
   allocate(innov(nrobs))
   allocate (R(nrobs,nrobs))

   call prep_4_EnKF(obs,A,nrobs,E,D,S,innov,R)

   print *,'----------------------------------------------------------'
   print *,'EnKF: Innovations of forecast ensemble'
   call innovation(obs,ave,var,nrobs,'F')

   if (checkobs) then
      allocate(hphstd(nrobs),obsstd(nrobs))

      do iens=1,nrens
      do m=1,nrobs
         hphstd(m)=hphstd(m)+S(m,iens)*S(m,iens)
      enddo
      enddo

      hphstd=hphstd/real(nrens-1)
      hphstd=sqrt(hphstd+tiny(hphstd))
      obsstd=sqrt(obs%var+tiny(obs%var))

      do m=1,nrobs
         if ( abs(innov(m)) > alpha*(hphstd(m)+obsstd(m)) ) then
            if (lfirst) then
               lfirst=.false.
               print '(1x,a10,a6,2a8,5a13)','         ',&
                                         '     '    ,&  
                                         '       '  ,&  
                                         '       '  ,&  
                                         ' obs%d      ',&  
                                         ' ens%d      ',&  
                                         ' obs%std    ',&  
                                         ' ens%std    ',&  
                                         ' new%std    '
            endif


  
            stdx= 2.0*(abs(innov(m))/alpha - hphstd(m))
            print '(1x,a,i6,2a8,5g13.5)','Outlayer: ',m,   &
                                   trim(obs(m)%wgname), &
                                   trim(obs(m)%keywrd), &
                                   obs(m)%d,            &
                                   obs(m)%d-innov(m),   &
                                   obsstd(m),           &
                                   hphstd(m),           &
                                   stdx
            
            obs(m)%var=stdx**2
         endif
      enddo
      lfirst=.true.

      call prep_4_EnKF(obs,A,nrobs,E,D,S,innov,R)
   endif





   if (verbose) then
!      print *,'----------------------------------------------------------'
!      print *,'EnKF: Verbose output'

!      do iens=1,1
!         print *,'EnKF: member ',iens
!         do m=1,nrobs
!            print '(a,i3,3(a,f10.3))',' EnKF: obs',m,': E=',E(m,iens),' D=',D(m,iens),' S=',S(m,iens)
!         enddo
!      enddo

      open(10,file='S1.dat')
         do i=1,nrens
            write(10,'(100f12.2)')S(1:nrobs,i)
         enddo
      close(10)

      open(10,file='D1.dat')
         do i=1,nrens
            write(10,'(100f12.2)')D(1:nrobs,i)
         enddo
      close(10)

      open(10,file='E1.dat')
         do i=1,nrens
            write(10,'(100f12.2)')E(1:nrobs,i)
         enddo
      close(10)
!      print *,'EnKF: End of verbose output'
!      print *,'----------------------------------------------------------'
   endif

   scale=1./real(nrobs*nrens)
   write(*,'(tr1,a,3g12.3)') 'forecast: innovation       (m < D < M) :',minval(D),sum(D)*scale,maxval(D)
   write(*,'(tr1,a,3g12.3)') 'forecast: ensemble anomaly (m < S < M) :',minval(S),sum(S)*scale,maxval(S)
   write(*,'(tr1,a,3g12.3)') 'forecast: obs perturbation (m < E < M) :',minval(E),sum(E)*scale,maxval(E)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Scale matrices if multiple observation types are used

   if (lscale) then
   print *,'----------------------------------------------------------'
   print *,'EnKF: Scale matrices'
      do m=1,nrobs
         scaling(m)=1./sqrt(obs(m)%var) ! + 1./real(nrens)*sum(S(m,:)**2.))
         print '(a,a,a,g10.2)',' Scaling= ',obs(m)%wgname,obs(m)%keywrd,scaling(m)
         S(m,:)=scaling(m)*S(m,:)
         E(m,:)=scaling(m)*E(m,:)
         D(m,:)=scaling(m)*D(m,:)
         innov(m)=scaling(m)*innov(m)
      enddo

      do j=1,nrobs
      do i=1,nrobs
         R(i,j)=scaling(i)*R(i,j)*scaling(j)
      enddo
      enddo

   endif

!   print *,'Analytical R matrix (1:nrobs,1:nrobs)'
!   do m=1,min(12,nrobs)
!      print '(12f10.4)',R(m,1:min(12,nrobs))
!   enddo
   
!   R=matmul(E,transpose(E))/real(nrens-1)
!   print *,'Low rank R matrix (1:12,1:12)'
!   do m=1,min(12,nrobs)
!      print '(12f10.4)',R(m,1:min(12,nrobs))
!   enddo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Compute global analysis
   print *,'----------------------------------------------------------'


   if (lporoperm) then
      allocate (poroens (nactive,3,nrens))
      do j=1,nrens
         poroens(1:nactive,1,j)=A(j)%poro(1:nactive)
         poroens(1:nactive,2,j)=A(j)%permx(1:nactive)
         poroens(1:nactive,3,j)=A(j)%permz(1:nactive)
      enddo
   endif

   select case (trim(ana))
   case('11','12','13','21','22','23')
      truncation=0.99
      read(ana,'(i2.2)')mode_analysis
      update_randrot=.true.
      print *,'mode_analysis= ',mode_analysis
      if (mode_analysis < 11 .or. mode_analysis > 23 ) then
         stop 'EnKF: Error in mode_analysis calling analysis'
      endif
      if (lporoperm) then
         call analysis(poroens, R, E, S, D, innov, 3*nactive, nrens, nrobs, .true., truncation,&
                    mode_analysis, update_randrot)
      else
         call analysis(A, R, E, S, D, innov, global_ndim, nrens, nrobs, .true., truncation,&
                    mode_analysis, update_randrot)
      endif
   case ('2')
      call analysis2(A, D, R, S, global_ndim, nrens, nrobs, .true.)
      print *,'EnKF: analysis2 done'

   case ('4c')
      print *,'EnKF: call analysis4c'
      call analysis4c(A, R, S, innov, global_ndim, nrens, nrobs, .true.)
      print *,'EnKF: analysis4 done'

   case ('5c')
      print *,'EnKF: call analysis5c'
      call analysis5c(A, R, S, innov, global_ndim, nrens, nrobs, .true.)
      print *,'EnKF: analysis5c done'

   case ('6c')
      print *,'EnKF: call analysis6c'
      call analysis6c(A, E, S, innov, global_ndim, nrens, nrobs, .true.)
      print *,'EnKF: analysis6c done'

   case default
      print *,'EnKF: Invalid value of ana= ',trim(ana)
      stop
   end select

   if (lporoperm) then
      do j=1,nrens
         do i=1,nactive
            A(j)%poro(i) =max(poroens(i,1,j),0.00001)
            A(j)%permx(i)=max(poroens(i,2,j),0.0)
            A(j)%permz(i)=max(poroens(i,3,j),0.0)
         enddo
      enddo
      deallocate(poroens)
   endif


   if (logperm) then
      print *,'----------------------------------------------------------'
      print *,'EnKF: logperm is active'
      do iens=1,nrens
         if (iens==1) print *,'maxperm: ',iens,maxval(A(iens)%permx),maxloc(A(iens)%permx)
         A(iens)%permx=exp(min(A(iens)%permx,19.5))-1.0
         A(iens)%permz=exp(min(A(iens)%permz,19.5))-1.0
!         A(iens)%permx=exp(min(A(iens)%permx,9.5))-1.0
!         A(iens)%permz=exp(min(A(iens)%permz,9.5))-1.0
         if (iens==1) print *,'maxperm: ',iens,maxval(A(iens)%permx),maxloc(A(iens)%permx)
      enddo
   endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Check state for consistency and fix if necessary
   print *,'----------------------------------------------------------'
   print *,'EnKF: Checking state and fixing if necessary'
   call fixeclipse(A)

   print *,'----------------------------------------------------------'
   print *,'EnKF: Innovations of analysed ensemble'
   call ensmean(A,ave)
   call ensvar(A,ave,var)
   call innovation(obs,ave,var,nrobs,'A')

   print *,'----------------------------------------------------------'
   print *,'EnKF: Well statistics of analysed ensemble'
   call wellstat(ave,var,obs,nrobs)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Write analyzed ensemble to file ensembleA.uf
   print *,'----------------------------------------------------------'
   print *,'EnKF: Dumping analysed ensemble'

   inquire(file=ensAfile,exist=ex)
   if (ex) then
      command=' ' 
      command(1:3)='rm '
      len=len_trim(ensAfile)
      command(4:4+len)=trim(ensAfile)
      print *,'Excecuting: ',trim(command)
      call system(trim(command))
   endif
   iflg=open_ensemblefile(ensAfile,'new')
      rident='ANALYSIS '
      do iens=1,nrens
         mem4=A(iens)
         iflg=write_ensmem(iens,rident,mem4)
      enddo
   close(10)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Testing innovations of analysis
   print *,'----------------------------------------------------------'
   call prep_4_EnKF(obs,A,nrobs,E,D,S,innov,R)
   write(*,'(tr1,a,3g12.3)') 'analysis: ensemble anomaly (m < S < M) :',minval(S),sum(S)*scale,maxval(S)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   deallocate(A)
   deallocate(obs)
   deallocate (S)
   deallocate (D)
   deallocate (E)
   deallocate (innov,scaling)
   print *,'----------------------------------------------------------'
   print *,'EnKF: Analysis done....'
   print *,'----------------------------------------------------------'
end subroutine
end module
