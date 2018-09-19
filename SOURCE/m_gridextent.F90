module m_gridextent
   use mod_dimensions
   real minxpos
   real minypos
   real maxxpos
   real maxypos
   real xlength
   real ylength
   real dx
   real dy
   integer mx
   integer my
   real, dimension(nx,ny,nz) :: xpos,ypos,zpos
contains
subroutine gridextent(lm)
   use mod_localdefs
   use m_eclipse_read
   implicit none
   logical, intent(in) :: lm(nx,ny,nz)

! Variables for reading coordinate positions from ECLIPSE.FGRID
   character(len=13) :: basename='ECLIPSE.FGRID'
   integer,   allocatable :: DIMENS(:)
   character(len=10), allocatable :: RADIAL(:)
   character(len=10), allocatable :: GRIDUNIT(:)
   character(len=10), allocatable :: MAPUNITS(:)
   real*4, allocatable :: MAPAXES(:)

   character(len=8)        gridname(1)
   integer                 gridsize(1)
   character(len=4)        gridtype(1)
   logical ex


   integer cc(7)
   integer coords(7,nx*ny*nz)
   real*4 corners4(24)
   real corners(24,nx*ny*nz)

   character(len=80) fname


   integer i, j, k, m, ird
   real lendx, lendy

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Grid point locations read from *.FGRID
   fname(:)=' '
   i=len_trim(refpath)
   fname(1:i)=trim(refpath)
   j=len_trim(eclbase)
   fname(i+1:i+j)=trim(eclbase)
   fname(i+j+1:i+j+6)='.FGRID'
   print *,trim(fname)
   inquire(file=trim(fname),exist=ex)
   open(10,file=trim(fname),status='old',form='formatted',access='sequential')
   m=0
   do 
      read(10,'(t3,a8,t13,i11,t26,a4)',end=999)gridname(1),gridsize(1),gridtype(1)
      select case (trim(gridname(1)))
      case('DIMENS')
         allocate(DIMENS(gridsize(1)))
         ird=read_integer(gridsize(1),DIMENS,gridtype(1))
      case('GRIDUNIT')
         allocate(GRIDUNIT(gridsize(1)))
         ird=read_char(gridsize(1),GRIDUNIT,gridtype(1))
      case('MAPAXES')
         allocate(MAPAXES(gridsize(1)))
         ird=read_real(gridsize(1),MAPAXES,gridtype(1))
      case('MAPUNITS')
         allocate(MAPUNITS(gridsize(1)))
         ird=read_char(gridsize(1),MAPUNITS,gridtype(1))
      case('RADIAL')
         allocate(RADIAL(gridsize(1)))
         ird=read_char(gridsize(1),RADIAL,gridtype(1))
      case('COORDS')
         m=m+1
         ird=read_integer(gridsize(1),cc(:),gridtype(1))
         coords(:,m)=cc(:)
      case('CORNERS')
         ird=read_real(gridsize(1),corners4(:),gridtype(1))

         i=coords(1,m)
         j=coords(2,m)
         k=coords(3,m)

         xpos(i,j,k)=sum(corners4(1:22:3))/8.0
         ypos(i,j,k)=sum(corners4(2:23:3))/8.0
         zpos(i,j,k)=sum(corners4(3:24:3))/8.0

         corners(:,i)=corners4(:)


      case default
         print '(t3,a8,t13,i11,t26,a4)',gridname(1),gridsize(1),gridtype(1)
         print *,'gridextent: variable not found '
         return
      end select

      if (m==nx*ny*nz) exit

   enddo
 999  close(10)


   minxpos=minval(xpos, MASK = lm); print *,'minxpos= ',minxpos
   maxxpos=maxval(xpos, MASK = lm); print *,'maxxpos= ',maxxpos
   minypos=minval(ypos, MASK = lm); print *,'minypos= ',minypos
   maxypos=maxval(ypos, MASK = lm); print *,'maxypos= ',maxypos

   xlength=maxxpos-minxpos; lendx=xlength/float(min(nx,ny))
   ylength=maxypos-minypos; lendy=ylength/float(min(nx,ny))

   print *,'xlength, dx (ave) : ', xlength, xlength/float(nx)
   print *,'ylength, dy (ave) : ', ylength, ylength/float(ny)

   minxpos=minxpos-2.0*lendx
   minypos=minypos-2.0*lendy
   maxxpos=maxxpos+2.0*lendx
   maxypos=maxypos+2.0*lendy
   xlength=maxxpos-minxpos
   ylength=maxypos-minypos


   print '(a,i8)','The number of grid cells is: ',m

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Defining pseudo random grid according to grid point locations

   mx=nx+ny
   my=mx
   dx=xlength/float(mx-1)
   dy=ylength/float(my-1)

   print *,'mx,my,dx,dy: ',mx,my,dx,dy

end subroutine
end module

