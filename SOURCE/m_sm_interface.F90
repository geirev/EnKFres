module m_sm_interface
contains
subroutine sm_interface()
   use m_interface
   implicit none
   character(len=1) yn
   logical ex

   character(len=1)  cvar
   character(len=4)  cnum
   character(len=80) eclname
   character(len=80) ensname
   character(len=80) ensdir
   character(len=80) ecldir
   integer           iens
   integer           iopt

   print '(a)','Running interface interactively'

   inquire(file='interface.in',exist=ex)
   if (ex) then
      print *,'The following interface.in exists:'
      call system('cat interface.in')
      
      print *
      write(*,'(a)',advance='no')'Edit file (y/n) :'
      read(*,*)yn
      
      if (yn == 'y') then
         call system('vi interface.in')
      endif

      open(10,file='interface.in')
      read(10,'(a)')ensdir
      read(10,'(a)')ensname
      read(10,'(a)')ecldir
      read(10,'(a)')eclname
      read(10,'(a)')cvar
      read(10,'(a)')cnum
      read(10,'(i3.3)')iens
      read(10,'(i1)')iopt
      close(10)
      


   else
      write(*,*)'No interface.in file available: create one by supplying the following information:'
      print *

      write(*,'(a)',advance='no')'directory to read/write ensemble file (dir/) : '
      read(*,*)ensdir

      write(*,'(a)',advance='no')'ensemble file base name ( [ensname]Fnnnn.uf) : '
      read(*,*)ensname

      write(*,'(a)',advance='no')'directory to read/write eclipse file (dir/)  : '
      read(*,*)ecldir

      write(*,'(a)',advance='no')'eclipse file base name  ([eclname].Fnnnn)    : '
      read(*,*)eclname

      write(*,'(a)',advance='no')'identifier for ensemble file (A/F)           : '
      read(*,*)cvar

      write(*,'(a)',advance='no')'time counter (four digits) (e.g. 0002)       : '
      read(*,*)cnum

      write(*,'(a)',advance='no')'ensemble member                              : '
      read(*,*)iens

      write(*,'(a)',advance='no')'ecl2ens (1), ens2ecl (2), mkstatic (3)       : '
      read(*,*)iopt
   endif

   write(*,'(a)',advance='no')'Do you still want to run interface (y/n): '
   read(*,*)yn
   if (yn =='y') then
      print *,'Running interface'
      call interface(ensdir,ensname,ecldir,eclname,cvar,cnum,iens,iopt)
      print *,'interface done.'
   endif

end subroutine
end module
