module m_swapmem
contains
subroutine swapmem()
! overwrites one ensemble member with another
   use mod_dimensions
   use mod_states
   use m_get_nrens
   use m_ensio
   implicit none
   
   type(states4) mem4
   integer iread,iwrite,i
   character(len=80) filename
   character(len=9) rident
   integer reclA,reclB
   integer nxx,nyy,nzz,nww
   logical ex
   real uniform
   integer draw,specified,iflg
   character(len=1) :: yn='y'


   write(*,'(a)',advance='no')'ensemble file name: ' 
   read(*,*)filename


   inquire(file=trim(filename),exist=ex)
   if (.not.ex) then
      print *,'The ensemble file ',trim(filename),' does not exist'
      return
   endif

   iflg=open_ensemblefile(filename,'old')

   do
      write(*,'(/,a)',advance='no')'Ensemble member to replace (0 to exit): '
      read(*,*)iwrite
      if (iwrite == 0) then
         close(10)
         return
      endif
      if (iwrite < 1 .or. iwrite > nrens) then
         print '(a)','Invalid value of ensemble member'
         cycle
      endif

      do
         call random_number(uniform)
         iread = ceiling (uniform*nrens)
         if (iread /= iwrite) exit 
      enddo

      write(*,'(a,2(i3,a))',advance='no')&
          'Replace member ',iwrite,' by member (sugg. ',iread,' ) (0 to exit): '
      read (*,*)iread

      if (iread == 0) then
         close(10)
         return
      endif

      if (1 <= iread .and. iread <= nrens) then
         iflg=read_ensmem(iread,mem4)
         read(10,rec=1)rident
         iflg=write_ensmem(iwrite,rident,mem4)
      else
         print *,'invalid iread or iwrite'
         cycle
      endif
   enddo

end subroutine
end module

