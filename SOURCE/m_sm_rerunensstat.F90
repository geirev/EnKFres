module m_sm_rerunensstat
contains
subroutine sm_rerunensstat()
use mod_states
use mod_localdefs
use m_printhelp
use m_ensstat
implicit none
character(len=2) comm
character(len=100) command
character(len=80) fileold
character(len=80) filenew
character(len=80) filename
integer i,j,iflag
character(len=1) yn
logical ex

comm=' '
do 
   print '(a)','    ======================================================='
   print '(a)','    = Recompute statistics of ensemble files              ='
   print '(a)','    ======================================================='
   print '(a)','    =   a: reprocess all (ensbase)F*.uf                   ='
   print '(a)','    =   b: reprocess all (ensbase)A*.uf                   ='
   print '(a)','    =   c: statistics of user defined list of ensfiles    ='
   print '(a)','    =   h: help                                           ='
   print '(a)','    =   q: return to main menu                            ='
   print '(a)','    ======================================================='
   write(*,'(a)',advance='no')'Command: '
   read(*,*)comm

   select case (trim(comm))
   case('q')
      exit

   case('a')
      command(:)=' '
      command(1:3)='ls '
      i=len_trim(ensbase)
      command(3+1:3+i)=trim(ensbase)
      command(3+i+1:3+i+24)='F????.uf > ensstat.files'
      print *,'Executing command: ',trim(command)
      call system(trim(command))
      write(*,'(a)',advance='no')'Save Diag restart files (y/n)? '
      read(*,*)yn
      if (yn == 'y') then
         iflag=1
      else
         iflag=0
      endif
      call ensstat(iflag)

   case('b')
      command(:)=' '
      command(1:3)='ls '
      i=len_trim(ensbase)
      command(3+1:3+i)=trim(ensbase)
      command(3+i+1:3+i+24)='A????.uf > ensstat.files'
      print *,'Executing command: ',trim(command)
      call system(trim(command))
      write(*,'(a)',advance='no')'Save Diag restart files (y/n)? '
      read(*,*)yn
      if (yn == 'y') then
         iflag=1
      else
         iflag=0
      endif
      call ensstat(1)

   case('c')
      print '(a)','Will now process the files listed in ensstat.files:'
      inquire(file='ensstat.files',exist=ex)
      if (.not.ex) then
         print '(a)','empty ensstat.files'
      else
         print '(a)','+++++++++++++++++++++++++++++++++++++++++'
         print '(a)','+ ensstat.files +++++++++++++++++++++++++'
         call system('cat ensstat.files')
         print '(a)','+++++++++++++++++++++++++++++++++++++++++'
      endif

      write(*,'(a)',advance='no')'Edit ensstat.files (y/n)? '
      read(*,*)yn
      if (yn=='y') call system('vi ensstat.files')

      write(*,'(a)',advance='no')'Run enstat (y/n)? '
      read(*,*)yn

      if (yn == 'y') then
         write(*,'(a)',advance='no')'Save Diag restart files (y/n)? '
         read(*,*)yn
         if (yn == 'y') then
            iflag=1
         else
            iflag=0
         endif
         call ensstat(iflag)
      endif


   case('h')
      call printhelp('ensstat.txt')

   case default
      print '(a)','Invalid command: try again!'
   end select

enddo

end subroutine
end module
