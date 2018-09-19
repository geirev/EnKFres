module m_get_nrobs
contains
integer function get_nrobs()
! Reads the observations to be used for assimilation from the file
! observation.uf. Each element is of type measurement and will be 
! stored in the vector d defined below.

   use mod_measurement
   implicit none

   integer reclO             ! Record length for an observation type
   type(measurement) obs     ! measurements

   integer j
   logical ex

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Find nb of  measurements stored
   inquire(file='observations.uf',exist=ex)
   if (.not.ex) stop 'File "observations.uf" does not exist'
   inquire(iolength=reclO)obs
   open(10,file='observations.uf',form='unformatted',access='direct',recl=reclO)
   do j=1,1000000
      read(10,rec=j,err=200)obs
   enddo
   200 get_nrobs=j-1
end function get_nrobs
end module m_get_nrobs
