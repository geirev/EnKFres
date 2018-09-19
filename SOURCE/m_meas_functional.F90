module m_meas_functional
contains
! Generates the elements of the S matrix.
! In this application we have only well observations.
! By augmenting the model state with all of these well
! variables we can measure the model state using a direct
! measurement functional.

real function meas_functional(obs,mem,lnew)
   use mod_states
   use mod_measurement
   use mod_localdefs
   use m_getmask
   implicit none
   type(measurement),    intent(in) :: obs
   type(states),         intent(in) :: mem
   logical,              intent(in) :: lnew
   real, save, allocatable :: poro(:,:,:),perm(:,:,:)

! Type of measurement (THP, BHP, OPR, GPR, WPR, WCT, GOR)
   select case (trim(obs%keywrd))
   case ('WTHP','WTHPH')
      meas_functional = mem%THP(obs%iw)
   case ('WBHP','WBHPH')
      meas_functional = mem%BHP(obs%iw)
   case ('WOPR','WOPRH')
      meas_functional = mem%OPR(obs%iw)
   case ('WGPR','WGPRH')
      meas_functional = mem%GPR(obs%iw)
   case ('WWPR','WWPRH')
      meas_functional = mem%WPR(obs%iw)
   case ('WWCT','WWCTH')
      meas_functional = mem%WCT(obs%iw)
   case ('WGOR','WGORH')
      meas_functional = mem%GOR(obs%iw)
   case ('PORO')
      if (.not. allocated(poro)) then
         allocate(poro(nx,ny,nz))
         poro=0.0
      endif
      if (lnew) poro=unpack(mem%poro,lmask,poro)
      meas_functional = poro(obs%i,obs%j,obs%k)
   case ('PERM')
      if (.not. allocated(perm)) then
         allocate(perm(nx,ny,nz))
         perm=0.0
      endif
      if (lnew) perm=unpack(mem%permx,lmask,perm)
      meas_functional = perm(obs%i,obs%j,obs%k)
      if (logperm) then
         meas_functional=log(1.0+meas_functional)
      endif
   case default
      print *,'meas_functional: No match in select case for',obs%keywrd
      stop
  end select

end function meas_functional
end module m_meas_functional
