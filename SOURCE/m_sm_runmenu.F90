module m_sm_runmenu
contains
subroutine sm_runmenu()
use mod_states
use mod_localdefs
use m_printhelp
use m_runexperiment
implicit none
character(len=2) comm,comm2
character(len=100) command
character(len=80) fileold
character(len=80) filenew
character(len=80) filename
integer i,j
character(len=1) yn
logical ex
integer iens1,iens2

iens1=1
iens2=nrens

inquire(file='rundefaults.in',exist=ex)
if (ex) then
   open(10,file='rundefaults.in')
      read(10,'(i4.4)',err=100,end=100)initime
      read(10,'(i4.4)',err=100,end=100)fintime
      read(10,'(a)',  err=100,end=100)lenkfana
      read(10,'(l1)',  err=100,end=100)pert_rates
      read(10,'(f4.2)',err=100,end=100)orat_pert
      read(10,'(f4.2)',err=100,end=100)wrat_pert
      read(10,'(f4.2)',err=100,end=100)grat_pert
   100 close(10)
endif

comm=' '
do 
   print '(a)',   '    ================================================================ '
   print '(a)',   '    = Modify parameters by typing menu number '
   print '(a,i4)','    =  a: Start time        =  ',initime
   print '(a,i4)','    =  b: Final time        =  ',fintime
   print '(a)',   '    = go: Start simulation'
   print '(a)',   '    =  h: Help'
   print '(a)',   '    =  q: return'
   print '(a)',   '    = ----- special flags -----------------'
   print '(a,a)', '    =  c: Analysis scheme (11, 12, 13, 21, 22, 23)       = ',lenkfana
   print '(a,l1)','    =  d: Spinup simulation (do not store intermediates) = ',spinup
   print '(a,l1)','    =  e: Skip ensemble integration, proceed to analysis = ',skipensint
   print '(a,l1)','    =  f: Exit after submitting ensemble, (debug option) = ',subexit
   print '(a,l1)','    =  g: Ensemble integration without analysis updates  = ',noana
   print '(a,l1,3f5.2)',&
                  '    =  i: Perturb rates                    = ',pert_rates,orat_pert,wrat_pert,grat_pert
   print '(a,i3.3,1x,i3.3)','    =  j: Integrate members iens1 to iens2              = ',iens1,iens2
   print '(a)',   '    ================================================================ '

   write(*,'(a)',advance='no')'Command: '
   read(*,*)comm

   select case (trim(comm))
   case('q')
      exit

   case('a')
      write(*,'(a)',advance='no')'New value for start time: '
      read(*,*)initime

   case('b')
      write(*,'(a)',advance='no')'New value for final time: '
      read(*,*)fintime

   case('go')
      call runexperiment(iens1,iens2)

   case('c')
      write(*,'(a)',advance='no')'EnKF analysis scheme (11, 12, 13, 21, 22, 23): '
      read(*,*)lenkfana
      
   case('d')
      write(*,'(a)')'Switching...: '
      if (spinup) then
         spinup=.false.
      else
         spinup=.true.
      endif

   case('e')
      write(*,'(a)')'Switching...: '
      if (skipensint) then
         skipensint=.false.
      else
         skipensint=.true.
      endif

   case('f')
      write(*,'(a)')'Switching...: '
      if (subexit) then
         subexit=.false.
      else
         subexit=.true.
      endif

   case('g')
      write(*,'(a)')'Switching...: '
      if (noana) then
         noana=.false.
      else
         noana=.true.
      endif

   case('i')
      write(*,'(a)')'Switching...: '
      if (pert_rates) then
         pert_rates=.false.
      else
         pert_rates=.true.
         comm2=' '
         do
            print '(a)',     '    ................................................................ '
            print '(a)',     '    . Modify perturbation parameters                                 '
            print '(a,f5.2)','    .  o:  New oil   rate perturbation (in % of ORAT) : ',orat_pert
            print '(a,f5.2)','    .  w:  New water rate perturbation (in % of WRAT) : ',wrat_pert
            print '(a,f5.2)','    .  g:  New gas   rate perturbation (in % of GRAT) : ',grat_pert
            print '(a)',     '    .  p:  Edit prepobs.def to update measurement errors             '
            print '(a)',     '    .  q: return                                                     '
            print '(a)',     '    ................................................................ '

            write(*,'(a)',advance='no')'Command: '
            read(*,*)comm2

            select case (trim(comm2))
            case('q')
               exit

            case('o')
               write(*,'(a)',advance='no')'New value for oil rate perturbation : '
               read(*,*)orat_pert

            case('w')
               write(*,'(a)',advance='no')'New value for water rate perturbation : '
               read(*,*)wrat_pert

            case('g')
               write(*,'(a)',advance='no')'New value for gas rate perturbation : '
               read(*,*)grat_pert

            case('p')
               call system('vi prepobs.def')

            case default
               print '(a)','Invalid command: try again!'
            end select

         enddo
      endif

   case('j')
      write(*,'(a)',advance='no')'First member to integrate: '
      read(*,*)iens1

      write(*,'(a)',advance='no')'Last member to integrate: '
      read(*,*)iens2

   case('h')
      call printhelp('runmenu.txt')

   case default
      print '(a)','Invalid command: try again!'
   end select

   open(10,file='rundefaults.in')
      write(10,'(i4.4)')initime
      write(10,'(i4.4)')fintime
      write(10,'(a)'  )lenkfana
      write(10,'(l1)'  )pert_rates
      write(10,'(f4.2)')orat_pert
      write(10,'(f4.2)')wrat_pert
      write(10,'(f4.2)')grat_pert
   close(10)

enddo

end subroutine
end module
