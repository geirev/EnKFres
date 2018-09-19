program main
! F90 program for EnKF_ECLIPSE.
use mod_dimensions
use mod_states
use mod_localdefs
use m_initialcheck
use m_printhelp
use m_getmask
use m_set_random_seed2

use m_sm_displayfiles
use m_sm_initialization
use m_sm_mkschedule
use m_sm_mkprior
use m_sm_referencecase
use m_sm_rmthincells
use m_sm_mkincludes
use m_sm_postrefcase
use m_sm_iniensemble
use m_sm_condiniensemble
use m_sm_iniensstat
use m_sm_rerunensstat
use m_sm_interface
use m_sm_mkprepobs
use m_sm_runmenu
use m_sm_diagnostics
use m_sm_swapmem

implicit none


character(len=3) comm      ! menu command

call set_random_seed2()

print *,'Initial check'
call initialcheck()

print *,'Get mask'
call getmask()

! Menu constructs
comm=' '
do 
   print '(a)','    ####################################################'
   print '(a)','    #       EnKF-Eclipse version 3.1 (15/4 2006)       #'
   print '(a)','    ####################################################'
   print '(a)','    #   a: Display user defined parameters      # help #'
   print '(a)','    #   b: Display system generated files       # cmds #'
   print '(a)','    #   v: Version updates                      #      #'
   print '(a)','    ####################################################'
   print '(a)','    # Initialization                                   #'
   print '(a)','    #   0: List required steps for initialization      #'
   print '(a)','    #   1: Generate new SCHEDULE_orig.INC file    (h1) #'
   print '(a)','    #   2: Generate poro/perm prior for refcase   (h2) #'
   print '(a)','    #   3: Run refcase                            (h3) #'
   print '(a)','    #   4: Update interface program               (h4) #'
   print '(a)','    #   5: Post processing of refcase             (h5) #'
   print '(a)','    #   6: Create initial ensemble                (h6) #'
   print '(a)','    #   7: Condition initial ensemble on log data (h7) #'
   print '(a)','    #   8: Compute statistics of initial ensemble (h8) #'
   print '(a)','    #   9: Generate new prepobs.def file          (h9) #'
   print '(a)','    ####################################################'
   print '(a)','    # Running assimilation experiment                  #'
   print '(a)','    #  10: Launch menu for ensemble integration        #'
   print '(a)','    ####################################################'
   print '(a)','    #  11: Diagnostics                                 #'
   print '(a)','    ####################################################'
   print '(a)','    # Special commands                                 #'
   print '(a)','    #   R: remove thin grid cells                      #'
   print '(a)','    #   E: run ensstat on specific files               #'
   print '(a)','    #   I: run interface interactively                 #'
   print '(a)','    #   S: Swap or replace an ensemble member          #'
   print '(a)','    #   h: Help                                        #'
   print '(a)','    #   q: Quit                                        #'
   print '(a)','    ####################################################'
   write(*,'(a)',advance='no')'Command: '
   read(*,*)comm 

   select case (trim(comm))
   case('q')
      exit

   case default
      print '(a)','Invalid command: try again!'

   case('a')
      call printlocaldefs()

   case('b')
      call sm_displayfiles()

   case('v')
      call printhelp('version.txt')

   case('0')
      call sm_initialization()

   case('1')
      call sm_mkschedule()

   case('h1')
      call printhelp('schedule_orig.txt')

   case('2')
      call sm_mkprior()

   case('h2')
      call printhelp('poroperm.txt')

   case('3')
      call sm_referencecase()

   case('h3')
      call printhelp('referencecase.txt')

   case('4')
      call sm_mkincludes()

   case('h4')
      call printhelp('includefiles.txt')

   case('5')
      call sm_postrefcase()

   case('h5')
      call printhelp('postprocessing.txt')

   case('6')
      if (.not.maskread) print *,'You need to read mask first (menu 5a)'
      call sm_iniensemble()

   case('h6')
      call printhelp('initialization.txt')

   case('7')
      if (.not.maskread) print *,'You need to read mask first (menu 5a)'
      call sm_condiniensemble()

   case('h7')
      call printhelp('conditioning.txt')

   case('8')
      if (.not.maskread) print *,'You need to read mask first (menu 5a)'
      call sm_iniensstat()

   case('h8')
      call printhelp('iniensstat.txt')

   case('9')
      call sm_mkprepobs()

   case('h9')
      call printhelp('prepobs.txt')

   case('10')
      if (.not.maskread) print *,'You need to read mask first (menu 5a)'
      call sm_runmenu()

   case('h10')
      call printhelp('runmenu.txt')

   case('11')
      call sm_diagnostics()

   case('R')
      call sm_rmthincells()

   case('E')
      call sm_rerunensstat()

   case('I')
      call sm_interface()

   case('S')
      call sm_swapmem()

   case('h')
      call printhelp('helpfile.txt')

   end select


enddo
end program

