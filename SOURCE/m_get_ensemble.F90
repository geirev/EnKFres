module m_get_ensemble
! Reads the ensemble forecast from file and stores it in the matrix A.
contains
subroutine get_ensemble(filename,A)
   use mod_dimensions
   use mod_states
   use m_ensio
   implicit none
   character(len=*), intent(in)  :: filename
   type(states),     intent(out) :: A(nrens)      ! Ensemble matrix

   type(states4) mem4

   integer iostat
   integer j,iflg

   logical ex

   iflg=open_ensemblefile(filename,'old')

   do j=1,nrens
      iflg=read_ensmem(j,mem4) 
      A(j)=mem4
   enddo
   close(10)

   print '(a,i5,a,a)','get_ensemble: ', nrens,' ensemble members read from ',filename

end subroutine get_ensemble
end module m_get_ensemble
