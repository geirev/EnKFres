module m_togrid
contains
function togrid(nx,ny,nz,mask,field)
   implicit none
   integer, intent(in) :: nx
   integer, intent(in) :: ny
   integer, intent(in) :: nz
   real,    intent(in) :: field(nx*ny*nz)
   integer, intent(in) :: mask(nx*ny*nz)
   real     work(nx*ny*nz)
   real     togrid(nx,ny,nz)
   integer i,j

   j=0
   do i=1,nx*ny*nz
      if (mask(i)==1) then
         j=j+1
         work(i)=field(j)
      else 
         work(i)=0.0
      endif
   enddo
   togrid=reshape(work,(/nx,ny,nz/))
end function togrid
end module m_togrid
