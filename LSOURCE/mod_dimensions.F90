module mod_dimensions
! model grid dimensions
   integer, parameter :: nrens=20             ! Number of ensemble members
   integer, parameter :: nx=40                ! i-dimension of model grid
   integer, parameter :: ny=64                ! j-dimension of model grid
   integer, parameter :: nz=14                ! k-dimension of model grid
   integer, parameter :: nw=8                 ! number of wells in model
   integer, parameter :: nactive=27755        ! number of active gridcells.

   integer, parameter :: nrflts=6             ! Number of faults to estimate fault trans.
   integer, parameter :: eqldims=1            ! Number of equil regions

#ifdef WELLSTATE
   integer, parameter :: nXGRP=10000          ! >= to max dimension of XGRP
   integer, parameter :: nRSEG=150000         ! >= to max dimension of RSEG
   integer, parameter :: nXWEL=1200           ! >= to max dimension of XWEL
   integer, parameter :: nSCON=40000          ! >= to max dimension of SCON
   integer, parameter :: nXCON=60000          ! >= to max dimension of XCON
#endif


end module mod_dimensions
! nw should be choosen larger than the number of wells one would ever
! expect to be drilled for the particular application.  Nonexistent wells
! are not used in the program, and one avoids the situation where 
! the ensemble file would have to be regenerated every time the number of
! wells are changed.

! Nactive is defaulted to nx*ny*nz but can be set by the user to the
! real number of active points to save disk space in the ensemble files.
! This is not properly tested yet.

! reclS is a parameter defining a maximum record length for the 
! static ensemble members stored in the ensembleS????.uf files.
! It is used in the io routines in mod_eclvars.F90.  It was needed
! in a one application where eclipse stored restart files of different
! size for different ensemble members and a common computed record length
! could not be used for all the static members.  

