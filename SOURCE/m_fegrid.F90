module m_fegrid
contains
subroutine fegrid(mask,maskread)
   use mod_dimensions
   use mod_localdefs
   use m_eclipse_read
   implicit none
   integer, intent(out) :: mask(nx,ny,nz)
   logical, intent(out) :: maskread

! Eclipse FEGRID header variables (ECLIPSE.FEGRID)
   integer                 ihead,iactdim
   logical              :: lhead=.false.
   integer, parameter   :: nrhead=8
   character(len=8)        headname(nrhead)
   integer                 headsize(nrhead)
   character(len=4)        headtype(nrhead)

   integer, allocatable :: FILEHEAD(:)
   character(len=10), allocatable :: MAPUNITS(:)
   real*4,  allocatable :: MAPAXES(:)
   character(len=10), allocatable :: GRIDUNIT(:)
   integer, allocatable :: GRIDHEAD(:)
   real*4,  allocatable :: COORD(:)
   real*4,  allocatable :: ZCORN(:)
   integer, allocatable :: ACTNUM(:)

   integer igrd,iact,i,j,ird

   character(len=80) filename
   character(len=20)FMT

   i=len_trim(refdir)
   j=len_trim(eclbase)
   filename(:)=' '
   filename(1:i)=trim(refdir)
   filename(i+1:i+j)=trim(eclbase)
   filename(i+j+1:i+j+7)='.FEGRID'
   print *,'fegrid: searching for file +++',trim(filename),'+++'
   inquire(file=trim(filename),exist=lhead)
   if (.not.lhead) then
      print *,'fegrid: ',trim(filename),' does not exist.'
      print *,'fegrid: the mask will be generated later after you run the reference case.'
   endif

   if (lhead) then
      open(10,file=trim(filename),form='formatted',access='sequential')
      ihead=0; igrd=0; iact=0
      do
         ihead=ihead+1
         read(10,'(t3,a8,t13,i11,t26,a4)',end=998)headname(ihead),headsize(ihead),headtype(ihead)
         print *,'HEADER: ',headname(ihead),headsize(ihead),headtype(ihead)

         select case (trim(headname(ihead)))
         case('FILEHEAD')
            allocate(FILEHEAD(headsize(ihead)))
            ird=read_integer(headsize(ihead),FILEHEAD,headtype(ihead))

         case('MAPUNITS')
            allocate(MAPUNITS(headsize(ihead)))
            ird=read_char(headsize(ihead),MAPUNITS,headtype(ihead))

         case('MAPAXES')
            allocate(MAPAXES(headsize(ihead)))
            ird=read_real(headsize(ihead),MAPAXES,headtype(ihead))

         case('GRIDUNIT')
            allocate(GRIDUNIT(headsize(ihead)))
            ird=read_char(headsize(ihead),GRIDUNIT,headtype(ihead))

         case('GRIDHEAD')
            igrd=1
            allocate(GRIDHEAD(headsize(ihead)))
            ird=read_integer(headsize(ihead),GRIDHEAD,headtype(ihead))

         case('COORD')
            allocate(COORD(headsize(ihead)))
            ird=read_real(headsize(ihead),COORD,headtype(ihead))

         case('ZCORN')
            allocate(ZCORN(headsize(ihead)))
            ird=read_real(headsize(ihead),ZCORN,headtype(ihead))

         case('ACTNUM')
            iact=1
            iactdim=headsize(ihead)
            allocate(ACTNUM(headsize(ihead)))
            ird=read_integer(headsize(ihead),ACTNUM,headtype(ihead))

            FMT(:)=' '
            write(FMT,'( "(",I0, "I1)"  )')nx
            print *,'FMT=',FMT
            do i=1,nz
               print *,'Actnum layer: ',i
               print FMT,ACTNUM((i-1)*nx*ny+1:i*nx*ny)
!               print '(46I1)',ACTNUM((i-1)*nx*ny+1:i*nx*ny)
            enddo
           

         case('ENDGRID')
            print *,'ENDGRID'
            exit
         case default
            print '(2a)','fegrid: variable not contained in select in fegrid.F90 : ',headname(ihead)
            stop
         end select
      enddo

 998  close(10)

      if (iact == 0 .or. igrd==0) then
         print *,'ERROR: fegrid file does not contain ACTNUM or GRIDHEAD: ',iact,igrd
         stop
      endif

       
      print *,'fegrid: mask.uf is being generated.'
      open(10,file='mask.uf',form='unformatted')
         write(10)GRIDHEAD(2:4)
         write(10)ACTNUM(1:iactdim)
      close(10)
      print *,'fegrid: mask.uf has been generated.'

      if (iactdim== nx*ny*nz) then
         mask=reshape(ACTNUM(1:iactdim),(/nx,ny,nz/))
         print *,'fegrid: reshape'
         maskread=.true.
      else
         print *,'fegrid: Dimension of ACTNUM does not match nx, ny, nz'
      endif
!      deallocate(ACTNUM,ZCORN,COORD,GRIDHEAD,GRIDUNIT,MAPAXES,MAPUNITS,FILEHEAD)
      print *,'fegrid: end of fegrid.'
   endif
         
end subroutine
end module

