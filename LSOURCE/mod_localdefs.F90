module mod_localdefs
use mod_dimensions

character(len=80), parameter :: version='rogaland'     ! Special custimization for hydro, statoil and rogaland
character(len=80), parameter :: eclbase='IN3'      ! Eclipse base name
character(len=80), parameter :: ensbase='ensemble'     ! ensemble file base name

logical            :: lfacies=.false.                  ! Set to true if the specific facies simulation is used.
integer            :: nre=1                            ! Order of sampling scheme
logical            :: logperm=.true.                   ! Logperm mode lognormal transformation on permeability.
integer            :: initime=0                        ! Default starttime
integer            :: fintime=310                      ! Default endtime

character(len=80), parameter :: refdir='Refcase/'       ! Path to directory for reference case
character(len=80), parameter :: pridir='Prior/'         ! Path to directory for priors

character(len=80), parameter :: enkfdir='/home/geve/Progs/EnKFres/Run'
character(len=80), parameter :: codedir='/home/geve/Progs/EnKFres/Code/EnKF'    
character(len=80), parameter :: helpdir='/home/geve/Progs/EnKFres/Helps/'    
character(len=80), parameter :: scriptdir='/home/geve/Progs/EnKFres/Scripts/'    

! run menu defaults
logical  :: spinup=.false.     ! Ensemble spinup from $initime to $fintime without stops.
logical  :: subexit=.false.    ! Exit script after submitting jobs (for testing).
logical  :: skipensint=.false. ! Skips ensemble integration and proceeds with next analysis.
character(len=2)  :: lenkfana='11'   ! Version of EnKF analysis scheme (11, 12, 13, 21, 22, 23)
                                     ! Old routines (2, 4c, 5c, 6c)   11=2, 21=4c, 22=5c, 23=6c
logical  :: noana=.false.      ! Skips analysis updates for ensemble integrations.
logical  :: pert_rates=.false. ! Add perturbations to rates in SCHEDULE.INC.
real     :: orat_pert=0.10     ! Default value for oil rate perturbations.
real     :: wrat_pert=0.10     ! Default value for water rate perturbations.
real     :: grat_pert=0.10     ! Default value for gas rate perturbations.


! Define statements
!character(len=80) :: defseg='-DDEFSEGMENT'
character(len=80) :: defseg=' '
character(len=80) :: defini=' '
character(len=80) :: defres=' '


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Do not change variables below this line
integer, parameter :: jfirst=1      ! First ensemble member to integrate.
integer, parameter :: jlast=nrens   ! Last ensemble member to integrate.
integer, parameter :: jjobs=jlast-jfirst+1


contains
subroutine printlocaldefs
   print '(a)','The following are preset in mod_localdefs.F90:'
   print '(a,a)',  ' Basename of ensemble file is    : ',trim(ensbase)
   print '(a,a)',  ' Basename of Eclipse  file is    : ',trim(eclbase)
   print '(a,i4)', ' Number of ensemble members is   : ',nrens
   print '(a,l1)', ' Facies mode is (F-off, T-on)    : ',lfacies
   print '(a,i1)', ' Improved sampling mode (nre) is : ',nre
   print '(a,l1)', ' Logperm mode (T/F)              : ',logperm
   print '(a,a)',  ' Location of help files          : ',trim(helpdir)
   print '(a,a)',  ' Location of scripts             : ',trim(scriptdir)
   print '(a,a)',  ' Version customized for          : ',trim(version)
   print *,' '
end subroutine
end module mod_localdefs
