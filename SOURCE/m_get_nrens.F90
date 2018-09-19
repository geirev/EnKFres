module m_get_nrens
contains
integer function get_nrens(filename)
   use mod_dimensions
   use mod_states
   use m_ensio
   implicit none
   type(states4) mem4 

   character(len=9) rident,rident1
   integer iostat
   character(len=*) filename

   integer reclA     
   integer reclB     

   integer iens,iflg,nn
   logical ex


! Open ensemble file
   iflg=open_ensemblefile(filename,'old')

! Count number of records
   do iens=1,1000
      read(10,rec=iens,err=100)rident
      if (iens == 1) then
         rident1=rident
      else
         if (rident /= rident1) then
            print *,'get_nrens: WARNING or ERROR.'
            print *,'  rident in member ',iens,' =',rident
            print *,'  is different from rident in member one=',rident1
            print *,'  ERROR: Could be caused by erroneous ensemble file.'
            print *,'  ERROR: Ensemble member may be missing.'
            print *,'  WARNING: rident may have been overwritten by another application.'
         endif
      endif
   enddo
   100 get_nrens=iens-1

   close(10)


end function get_nrens
end module m_get_nrens
