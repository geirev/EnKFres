module m_readrft
contains
subroutine readrft(iens,cnum)
   use mod_localdefs
   use m_eclipse_read
   implicit none
   integer, intent(in) :: iens
   character(len=4), intent(in) :: cnum

   character(len=80) filename

! Eclipse FEGRID header variables (ECLIPSE.FEGRID)
   integer                 ihead
   integer, parameter   :: nrhead=20
   character(len=8)        headname(nrhead)
   integer                 headsize(nrhead)
   character(len=4)        headtype(nrhead)

   real*4,  allocatable :: TIME(:)
   integer, allocatable :: DATE(:)
   character(len=10), allocatable :: WELLETC(:)
   character(len=10), allocatable :: HOSTGRID(:)
   integer, allocatable :: CONIPOS(:)
   integer, allocatable :: CONJPOS(:)
   integer, allocatable :: CONKPOS(:)
   real*4,  allocatable :: DEPTH(:)
   real*4,  allocatable :: PRESSURE(:)
   real*4,  allocatable :: SWAT(:)
   real*4,  allocatable :: SGAS(:)

   character(len=4)        tag4
   integer i,j,ird,isize
   logical ltime
   logical :: ex=.false.
   integer reclA

   ltime=.false.

   filename(:)=' '
   filename(1:7)='tmpdir_'
   write(tag4,'(i4.4)')iens
   filename(8:11)=tag4(1:4)
   filename(12:12)='/'
   j=len_trim(eclbase)
   filename(12+1:12+j)=eclbase(1:j)
   filename(12+j+1:12+j+5)='.FRFT'

!   print *,'readrft: searching for file +++',trim(filename),'+++'
   inquire(file=trim(filename),exist=ex)
   if (.not.ex) then
!      print *,'readrft: no rft file +++',trim(filename),'+++'
!      print *,'readrft: returns'
      return
   endif
   print *,'readrft: reading +++',trim(filename),'+++'

    
   open(10,file=trim(filename),form='formatted',access='sequential')
   ihead=0
   do
      ihead=ihead+1
      read(10,'(t3,a8,t13,i11,t26,a4)',end=998)headname(ihead),headsize(ihead),headtype(ihead)
      print *,'HEADER: ',headname(ihead),headsize(ihead),headtype(ihead)

      select case (trim(headname(ihead)))
      case('TIME')
         if (ltime) then
            print *,'readrft: One time record already read.'
            print *,'readrft: You need to implement support for the assimilation of'
            print *,'readrft: more than one rft record occuring at the same time.'
            goto 998
         endif
         ltime=.true.
         allocate(TIME(headsize(ihead)))
         ird=read_real(headsize(ihead),TIME,headtype(ihead))

      case('DATE')
         allocate(DATE(headsize(ihead)))
         ird=read_integer(headsize(ihead),DATE,headtype(ihead))

      case('WELLETC')
         allocate(WELLETC(headsize(ihead)))
         ird=read_char(headsize(ihead),WELLETC,headtype(ihead))

      case('HOSTGRID')
         allocate(HOSTGRID(headsize(ihead)))
         ird=read_char(headsize(ihead),HOSTGRID,headtype(ihead))

      case('CONIPOS')
         isize=ihead
         allocate(CONIPOS(headsize(ihead)))
         ird=read_integer(headsize(ihead),CONIPOS,headtype(ihead))

      case('CONJPOS')
         allocate(CONJPOS(headsize(ihead)))
         ird=read_integer(headsize(ihead),CONJPOS,headtype(ihead))

      case('CONKPOS')
         allocate(CONKPOS(headsize(ihead)))
         ird=read_integer(headsize(ihead),CONKPOS,headtype(ihead))

      case('DEPTH')
         allocate(DEPTH(headsize(ihead)))
         ird=read_real(headsize(ihead),DEPTH,headtype(ihead))

      case('PRESSURE')
         allocate(PRESSURE(headsize(ihead)))
         ird=read_real(headsize(ihead),PRESSURE,headtype(ihead))

      case('SWAT')
         allocate(SWAT(headsize(ihead)))
         ird=read_real(headsize(ihead),SWAT,headtype(ihead))

      case('SGAS')
         allocate(SGAS(headsize(ihead)))
         ird=read_real(headsize(ihead),SGAS,headtype(ihead))

      case default
         print *,'variable not contained in select in fegrid.F90 : ',headname(ihead)
         stop
      end select
   enddo
 998  close(10)

    inquire(iolength=reclA)headsize(isize),CONIPOS,CONJPOS,CONKPOS,DEPTH,PRESSURE,SWAT,SGAS
    open(10,file='ensrft'//cnum//'.uf',form='unformatted',access='direct',recl=reclA)
       write(10,rec=iens)headsize(isize),CONIPOS,CONJPOS,CONKPOS,DEPTH,PRESSURE,SWAT,SGAS
    close(10)

end subroutine
end module

