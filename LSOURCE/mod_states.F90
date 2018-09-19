module mod_states
! Modelstate definition for ECLIPSE
   use mod_dimensions

   type states
! Solution data
      real pres(nactive)        ! 3-D pressure
      real Sg(nactive)          ! 3-D gas-saturation
      real Sw(nactive)          ! 3-D water-saturation
      real Rs(nactive)          ! 3-D dissolved gas-oil ratio
#ifdef VAPOIL
      real Rv(nactive)         ! 3-D dissolved oil-gas ratio
#endif
! Static data
      real permx(nactive)       ! 3-D permxeability
      real permz(nactive)       ! 3-D permzeability
      real poro(nactive)        ! 3-D porosity

#ifdef MULTZ
      real multz(nz-1)           ! Vertical transmissibility multiplicators
#endif

#ifdef MULTFLT
      real multflt(nrflts)       ! Fault transmissibility multiplicators
#endif

#ifdef EQUIL
      real equilWOC(eqldims)     ! initial WOC contacts for each member.
      real equilGOC(eqldims)     ! initial GOC contacts for each member.
#endif

#ifdef GAUSS2
      real gauss1(nx,ny,nz)
      real gauss2(nx,ny,nz)
#endif

#ifdef WELLSTATE
! Well solution data
      real XGRP(nXGRP)           ! Data for well groups
      real RSEG(nRSEG)           ! Data for segment wells
      real XWEL(nXWEL)           ! Well data
      real SCON(nSCON)           ! Static perforation data
      real XCON(nXCON)           ! Well perforation data
#endif

! Diagnostic data used in assimilation
      real THP(nw)               ! Top hole pressure
      real BHP(nw)               ! Bottom hole pressure
      real OPR(nw)               ! Oil rate
      real GPR(nw)               ! Gas rate
      real WPR(nw)               ! Water rate
      real WCT(nw)               ! Water cut {WPR /(WPR +OPR}
      real GOR(nw)               ! Gas Oil Rate {GPR /OPR}

! Some additional diagnostic data for plotting
      real OPT(nw)               ! Accumulated oil production
      real GPT(nw)               ! Accumulated gas production
      real WPT(nw)               ! Accumulated water production

   end type states

   type states4
      real*4 pres(nactive)
      real*4 Sg(nactive)
      real*4 Sw(nactive)
      real*4 Rs(nactive)
#ifdef VAPOIL
      real*4 Rv(nactive)
#endif

      real*4 permx(nactive)
      real*4 permz(nactive)
      real*4 poro(nactive)

#ifdef MULTZ
      real*4 multz(nz-1)           ! Vertical transmissibility multiplicators
#endif

#ifdef MULTFLT
      real*4 multflt(nrflts)       ! Fault transmissibility multiplicators
#endif

#ifdef EQUIL
      real*4 equilWOC(eqldims)     ! initial WOC contacts for each member.
      real*4 equilGOC(eqldims)     ! initial GOC contacts for each member.
#endif

#ifdef GAUSS2
      real*4 gauss1(nx,ny,nz)
      real*4 gauss2(nx,ny,nz)
#endif

#ifdef WELLSTATE
      real*4 XGRP(nXGRP)
      real*4 RSEG(nRSEG)
      real*4 XWEL(nXWEL)
      real*4 SCON(nSCON)
      real*4 XCON(nXCON)
#endif

      real*4 THP(nw)
      real*4 BHP(nw)
      real*4 OPR(nw)
      real*4 GPR(nw)
      real*4 WPR(nw)
      real*4 WCT(nw)
      real*4 GOR(nw)

      real*4 OPT(nw)
      real*4 GPT(nw)
      real*4 WPT(nw)
   end type states4


! Overloaded and generic operators
   interface operator(+)
      module procedure add_states
   end interface

   interface operator(-)
      module procedure subtract_states
   end interface

   interface operator(*)
      module procedure states_real_mult,&
                       real_states_mult,&
                       states_states_mult
   end interface

!   interface operator(/)
!      module procedure divide_states
!   end interface

   interface assignment(=)
      module procedure assign_states
      module procedure assign_states4
      module procedure states4to8
      module procedure states8to4
   end interface

   interface sqrt
      module procedure sqrt_states
   end interface


contains

   function sqrt_states(A)
      type(states) sqrt_states
      type(states), intent(in) :: A
      real :: eps=0.1E-10
      sqrt_states%pres       = sqrt(A%pres+eps)
      sqrt_states%Sw         = sqrt(A%Sw+eps)
      sqrt_states%Sg         = sqrt(A%Sg+eps)

      sqrt_states%Rs         = sqrt(A%Rs+eps)
#ifdef VAPOIL
      sqrt_states%Rv         = sqrt(A%Rv+eps)
#endif

      sqrt_states%permx      = sqrt(A%permx+eps)
      sqrt_states%permz      = sqrt(A%permz+eps)
      sqrt_states%poro       = sqrt(A%poro+eps)
#ifdef MULTZ
      sqrt_states%multz      = sqrt(A%multz+eps)
#endif

#ifdef MULTFLT
      sqrt_states%multflt    = sqrt(A%multflt+eps)
#endif

#ifdef EQUIL
      sqrt_states%equilWOC   = sqrt(A%equilWOC+eps)
      sqrt_states%equilGOC   = sqrt(A%equilGOC+eps)
#endif

#ifdef GAUSS2
      sqrt_states%gauss1     = sqrt(A%gauss1+eps)
      sqrt_states%gauss2     = sqrt(A%gauss2+eps)
#endif

#ifdef WELLSTATE
      sqrt_states%XGRP       = sqrt(A%XGRP)
      sqrt_states%RSEG       = sqrt(A%RSEG)
      sqrt_states%XWEL       = sqrt(A%XWEL)
      sqrt_states%SCON       = sqrt(A%SCON)
      sqrt_states%XCON       = sqrt(A%XCON)
#endif

      sqrt_states%THP        = sqrt(A%THP)
      sqrt_states%BHP        = sqrt(A%BHP)
      sqrt_states%OPR        = sqrt(A%OPR)
      sqrt_states%GPR        = sqrt(A%GPR)
      sqrt_states%WPR        = sqrt(A%WPR)
      sqrt_states%WCT        = sqrt(A%WCT)
      sqrt_states%GOR        = sqrt(A%GOR)

      sqrt_states%OPT        = sqrt(A%OPT)
      sqrt_states%GPT        = sqrt(A%GPT)
      sqrt_states%WPT        = sqrt(A%WPT)

   end function sqrt_states

   function add_states(A,B)
      type(states) add_states
      type(states), intent(in) :: A
      type(states), intent(in) :: B
      add_states%pres       = A%pres + B%pres
      add_states%Sw         = A%Sw   + B%Sw
      add_states%Sg         = A%Sg   + B%Sg
      add_states%Rs         = A%Rs   + B%Rs
#ifdef VAPOIL
      add_states%Rv         = A%Rv   + B%Rv
#endif

      add_states%permx      = A%permx+ B%permx
      add_states%permz      = A%permz+ B%permz
      add_states%poro       = A%poro + B%poro
#ifdef MULTZ
      add_states%multz      = A%multz + B%multz
#endif

#ifdef MULTFLT
      add_states%multflt      = A%multflt + B%multflt
#endif

#ifdef EQUIL
      add_states%equilWOC      = A%equilWOC + B%equilWOC
      add_states%equilGOC      = A%equilGOC + B%equilGOC
#endif


#ifdef GAUSS2
      add_states%gauss1     = A%gauss1 + B%gauss1
      add_states%gauss2     = A%gauss2 + B%gauss2
#endif

#ifdef WELLSTATE
      add_states%XGRP       = A%XGRP + B%XGRP
      add_states%RSEG       = A%RSEG + B%RSEG
      add_states%XWEL       = A%XWEL + B%XWEL
      add_states%SCON       = A%SCON + B%SCON
      add_states%XCON       = A%XCON + B%XCON
#endif

      add_states%THP        = A%THP  + B%THP
      add_states%BHP        = A%BHP  + B%BHP
      add_states%OPR        = A%OPR  + B%OPR
      add_states%GPR        = A%GPR  + B%GPR 
      add_states%WPR        = A%WPR  + B%WPR 
      add_states%WCT        = A%WCT  + B%WCT
      add_states%GOR        = A%GOR  + B%GOR

      add_states%OPT        = A%OPT  + B%OPT
      add_states%GPT        = A%GPT  + B%GPT
      add_states%WPT        = A%WPT  + B%WPT
   end function add_states

   function subtract_states(A,B)
      type(states) subtract_states
      type(states), intent(in) :: A
      type(states), intent(in) :: B
      subtract_states%pres  = A%pres - B%pres
      subtract_states%Sw    = A%Sw   - B%Sw
      subtract_states%Sg    = A%Sg   - B%Sg
      subtract_states%Rs    = A%Rs   - B%Rs
#ifdef VAPOIL
      subtract_states%Rv    = A%Rv   - B%Rv
#endif

      subtract_states%permx = A%permx- B%permx
      subtract_states%permz = A%permz- B%permz
      subtract_states%poro  = A%poro - B%poro
#ifdef MULTZ
      subtract_states%multz  = A%multz - B%multz
#endif

#ifdef MULTFLT
      subtract_states%multflt  = A%multflt - B%multflt
#endif

#ifdef EQUIL
      subtract_states%equilWOC  = A%equilWOC - B%equilWOC
      subtract_states%equilGOC  = A%equilGOC - B%equilGOC
#endif

#ifdef GAUSS2
      subtract_states%gauss1     = A%gauss1 - B%gauss1
      subtract_states%gauss2     = A%gauss2 - B%gauss2
#endif

#ifdef WELLSTATE
      subtract_states%XGRP       = A%XGRP - B%XGRP
      subtract_states%RSEG       = A%RSEG - B%RSEG
      subtract_states%XWEL       = A%XWEL - B%XWEL
      subtract_states%SCON       = A%SCON - B%SCON
      subtract_states%XCON       = A%XCON - B%XCON
#endif

      subtract_states%THP   = A%THP  - B%THP
      subtract_states%BHP   = A%BHP  - B%BHP
      subtract_states%OPR   = A%OPR  - B%OPR
      subtract_states%GPR   = A%GPR  - B%GPR 
      subtract_states%WPR   = A%WPR  - B%WPR 
      subtract_states%WCT   = A%WCT  - B%WCT 
      subtract_states%GOR   = A%GOR  - B%GOR 

      subtract_states%OPT   = A%OPT  - B%OPT
      subtract_states%GPT   = A%GPT  - B%GPT
      subtract_states%WPT   = A%WPT  - B%WPT
   end function subtract_states

   function states_real_mult(A,B)
      type(states) states_real_mult
      type(states), intent(in) :: A
      real, intent(in) :: B
      states_real_mult%pres = B*A%pres
      states_real_mult%Sw   = B*A%Sw
      states_real_mult%Sg   = B*A%Sg
      states_real_mult%Rs   = B*A%Rs
#ifdef VAPOIL
      states_real_mult%Rv   = B*A%Rv
#endif

      states_real_mult%permx= B*A%permx
      states_real_mult%permz= B*A%permz
      states_real_mult%poro = B*A%poro
#ifdef MULTZ
      states_real_mult%multz = B*A%multz
#endif

#ifdef MULTFLT
      states_real_mult%multflt = B*A%multflt
#endif

#ifdef EQUIL
      states_real_mult%equilWOC = B*A%equilWOC
      states_real_mult%equilGOC = B*A%equilGOC
#endif

#ifdef GAUSS2
      states_real_mult%gauss1     = B*A%gauss1
      states_real_mult%gauss2     = B*A%gauss2
#endif

#ifdef WELLSTATE
      states_real_mult%XGRP = B*A%XGRP
      states_real_mult%RSEG = B*A%RSEG
      states_real_mult%XWEL = B*A%XWEL
      states_real_mult%SCON = B*A%SCON
      states_real_mult%XCON = B*A%XCON
#endif

      states_real_mult%THP  = B*A%THP
      states_real_mult%BHP  = B*A%BHP
      states_real_mult%OPR  = B*A%OPR
      states_real_mult%GPR  = B*A%GPR 
      states_real_mult%WPR  = B*A%WPR 
      states_real_mult%WCT  = B*A%WCT
      states_real_mult%GOR  = B*A%GOR

      states_real_mult%OPT  = B*A%OPT
      states_real_mult%GPT  = B*A%GPT
      states_real_mult%WPT  = B*A%WPT
   end function states_real_mult

   function real_states_mult(B,A)
      type(states) real_states_mult
      type(states), intent(in) :: A
      real, intent(in) :: B

      real_states_mult%pres = B*A%pres
      real_states_mult%Sw   = B*A%Sw
      real_states_mult%Sg   = B*A%Sg
      real_states_mult%Rs   = B*A%Rs
#ifdef VAPOIL
      real_states_mult%Rv   = B*A%Rv
#endif

      real_states_mult%permx= B*A%permx
      real_states_mult%permz= B*A%permz
      real_states_mult%poro = B*A%poro
#ifdef MULTZ
      real_states_mult%multz = B*A%multz
#endif

#ifdef MULTFLT
      real_states_mult%multflt = B*A%multflt
#endif

#ifdef EQUIL
      real_states_mult%equilWOC = B*A%equilWOC
      real_states_mult%equilGOC = B*A%equilGOC
#endif

#ifdef GAUSS2
      real_states_mult%gauss1     = B*A%gauss1
      real_states_mult%gauss2     = B*A%gauss2
#endif

#ifdef WELLSTATE
      real_states_mult%XGRP = B*A%XGRP
      real_states_mult%RSEG = B*A%RSEG
      real_states_mult%XWEL = B*A%XWEL
      real_states_mult%SCON = B*A%SCON
      real_states_mult%XCON = B*A%XCON
#endif

      real_states_mult%THP  = B*A%THP
      real_states_mult%BHP  = B*A%BHP
      real_states_mult%OPR  = B*A%OPR
      real_states_mult%GPR  = B*A%GPR 
      real_states_mult%WPR  = B*A%WPR 
      real_states_mult%WCT  = B*A%WCT
      real_states_mult%GOR  = B*A%GOR

      real_states_mult%OPT  = B*A%OPT
      real_states_mult%GPT  = B*A%GPT
      real_states_mult%WPT  = B*A%WPT
   end function real_states_mult

   function states_states_mult(A,B)
      type(states) states_states_mult
      type(states), intent(in) :: A
      type(states), intent(in) :: B
      states_states_mult%pres = A%pres * B%pres
      states_states_mult%Sw   = A%Sw   * B%Sw
      states_states_mult%Sg   = A%Sg   * B%Sg
      states_states_mult%Rs   = A%Rs   * B%Rs
#ifdef VAPOIL
      states_states_mult%Rv   = A%Rv   * B%Rv
#endif

      states_states_mult%permx= A%permx* B%permx
      states_states_mult%permz= A%permz* B%permz
      states_states_mult%poro = A%poro * B%poro
#ifdef MULTZ
      states_states_mult%multz = A%multz * B%multz
#endif

#ifdef MULTFLT
      states_states_mult%multflt = A%multflt * B%multflt
#endif

#ifdef EQUIL
      states_states_mult%equilWOC = A%equilWOC * B%equilWOC
      states_states_mult%equilGOC = A%equilGOC * B%equilGOC
#endif

#ifdef GAUSS2
      states_states_mult%gauss1 = A%gauss1*B%gauss1
      states_states_mult%gauss2 = A%gauss2*B%gauss2
#endif

#ifdef WELLSTATE
      states_states_mult%XGRP = A%XGRP * B%XGRP
      states_states_mult%RSEG = A%RSEG * B%RSEG
      states_states_mult%XWEL = A%XWEL * B%XWEL
      states_states_mult%SCON = A%SCON * B%SCON
      states_states_mult%XCON = A%XCON * B%XCON
#endif

      states_states_mult%THP  = A%THP  * B%THP
      states_states_mult%BHP  = A%BHP  * B%BHP
      states_states_mult%OPR  = A%OPR  * B%OPR
      states_states_mult%GPR  = A%GPR  * B%GPR 
      states_states_mult%WPR  = A%WPR  * B%WPR 
      states_states_mult%WCT  = A%WCT  * B%WCT 
      states_states_mult%GOR  = A%GOR  * B%GOR 

      states_states_mult%OPT  = A%OPT  * B%OPT
      states_states_mult%GPT  = A%GPT  * B%GPT
      states_states_mult%WPT  = A%WPT  * B%WPT
   end function states_states_mult


   subroutine assign_states(A,r)
      type(states), intent(out) :: A
      real, intent(in) :: r
      A%pres = r
      A%Sw   = r
      A%Sg   = r
      A%Rs   = r
#ifdef VAPOIL
      A%Rv   = r
#endif

      A%permx= r
      A%permz= r
      A%poro = r
#ifdef MULTZ
      A%multz = r
#endif

#ifdef MULTFLT
      A%multflt = r
#endif

#ifdef EQUIL
      A%equilWOC=r
      A%equilGOC=r
#endif

#ifdef GAUSS2
      A%gauss1=r
      A%gauss2=r
#endif

#ifdef WELLSTATE
      A%XGRP = r
      A%RSEG = r
      A%XWEL = r
      A%SCON = r
      A%XCON = r
#endif

      A%THP  = r
      A%BHP  = r
      A%OPR  = r
      A%GPR  = r
      A%WPR  = r
      A%WCT  = r
      A%GOR  = r

      A%OPT  = r
      A%GPT  = r
      A%WPT  = r
   end subroutine assign_states

   subroutine assign_states4(A,r)
      type(states4), intent(out) :: A
      real, intent(in) :: r
      A%pres = r
      A%Sw   = r
      A%Sg   = r
      A%Rs   = r
#ifdef VAPOIL
      A%Rv   = r
#endif

      A%permx= r
      A%permz= r
      A%poro = r
#ifdef MULTZ
      A%multz = r
#endif

#ifdef MULTFLT
      A%multflt = r
#endif

#ifdef EQUIL
      A%equilWOC = r
      A%equilGOC = r
#endif

#ifdef GAUSS2
      A%gauss1=r
      A%gauss2=r
#endif

#ifdef WELLSTATE
      A%XGRP = r
      A%RSEG = r
      A%XWEL = r
      A%SCON = r
      A%XCON = r
#endif

      A%THP  = r
      A%BHP  = r
      A%OPR  = r
      A%GPR  = r
      A%WPR  = r
      A%WCT  = r
      A%GOR  = r

      A%OPT  = r
      A%GPT  = r
      A%WPT  = r
   end subroutine assign_states4

   subroutine states4to8(A,B)
      type(states), intent(out) :: A
      type(states4), intent(in)  :: B
      A%pres  = DBLE(B%pres)
      A%Sw    = DBLE(B%Sw)
      A%Sg    = DBLE(B%Sg)
      A%Rs    = DBLE(B%Rs)
#ifdef VAPOIL
      A%Rv    = DBLE(B%Rv)
#endif

      A%permx = DBLE(B%permx)
      A%permz = DBLE(B%permz)
      A%poro  = DBLE(B%poro)
#ifdef MULTZ
      A%multz  = DBLE(B%multz)
#endif

#ifdef MULTFLT
      A%multflt  = DBLE(B%multflt)
#endif

#ifdef EQUIL
      A%equilWOC= DBLE(B%equilWOC)
      A%equilGOC= DBLE(B%equilGOC)
#endif

#ifdef GAUSS2
      A%gauss1=DBLE(B%gauss1)
      A%gauss2=DBLE(B%gauss2)
#endif

#ifdef WELLSTATE
      A%XGRP  = DBLE(B%XGRP)
      A%RSEG  = DBLE(B%RSEG)
      A%XWEL  = DBLE(B%XWEL)
      A%SCON  = DBLE(B%SCON)
      A%XCON  = DBLE(B%XCON)
#endif

      A%THP   = DBLE(B%THP)
      A%BHP   = DBLE(B%BHP)
      A%OPR   = DBLE(B%OPR)
      A%GPR   = DBLE(B%GPR)
      A%WPR   = DBLE(B%WPR)
      A%WCT   = DBLE(B%WCT)
      A%GOR   = DBLE(B%GOR)

      A%OPT  = DBLE(B%OPT)
      A%GPT  = DBLE(B%GPT)
      A%WPT  = DBLE(B%WPT)
   end subroutine states4to8

   subroutine states8to4(A,B)
      type(states), intent(in)  :: B
      type(states4),  intent(out) :: A
      A%pres  = REAL(B%pres)
      A%Sw    = REAL(B%Sw)
      A%Sg    = REAL(B%Sg)
      A%Rs    = REAL(B%Rs)
#ifdef VAPOIL
      A%Rv    = REAL(B%Rv)
#endif

      A%permx = REAL(B%permx)
      A%permz = REAL(B%permz)
      A%poro  = REAL(B%poro)
#ifdef MULTZ
      A%multz  = REAL(B%multz)
#endif

#ifdef MULTFLT
      A%multflt  = REAL(B%multflt)
#endif

#ifdef EQUIL
      A%equilWOC= REAL(B%equilWOC)
      A%equilGOC= REAL(B%equilGOC)
#endif

#ifdef GAUSS2
      A%gauss1= REAL(B%gauss1)
      A%gauss2= REAL(B%gauss2)
#endif

#ifdef WELLSTATE
      A%XGRP  = REAL(B%XGRP)
      A%RSEG  = REAL(B%RSEG)
      A%XWEL  = REAL(B%XWEL)
      A%SCON  = REAL(B%SCON)
      A%XCON  = REAL(B%XCON)
#endif

      A%THP   = REAL(B%THP)
      A%BHP   = REAL(B%BHP)
      A%OPR   = REAL(B%OPR)
      A%GPR   = REAL(B%GPR)
      A%WPR   = REAL(B%WPR)
      A%WCT   = REAL(B%WCT)
      A%GOR   = REAL(B%GOR)

      A%OPT   = REAL(B%OPT)
      A%GPT   = REAL(B%GPT)
      A%WPT   = REAL(B%WPT)
   end subroutine states8to4

end module mod_states

