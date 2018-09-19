module m_prepobs
contains
subroutine prepobs(cnext)
! Reads data from different data sources and converts the
! information to the parameters stored in observations.uf
! Additional information is needed about the model grid.

   use mod_measurement
   use mod_dimensions
   use m_eclipse_read
   implicit none

   type obsinfo
      character(len=8) wgname    ! well name
      integer          iw        ! well number
      character(len=8) keywrd    ! name of observation variable
      logical          active    ! to be assimilated or not
      real             std       ! error standard dev 
      integer          ierr      ! error in % of data value
   end type obsinfo

   character(len=4),  intent(in) :: cnext


   integer, parameter :: maxobs = 10000
   type (obsinfo)        info(maxobs)
   type (measurement), allocatable ::    obs(:)

   integer i,m,ird,ilen,nrobs,iobs,inactive,nnobs,j,iwell
   logical ex

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Eclipse measurement file variables
   integer                 meastime
   character(len=80)       measfile
   integer                 imeas
   logical              :: lmeas=.false.
   integer, parameter   :: nrmeas=5
   character(len=8)        measname(nrmeas)
   integer                 meassize(nrmeas)
   character(len=4)        meastype(nrmeas)
   character(len=4)        cmeastime

   character(len=10), allocatable :: KEYWORDS(:)
   character(len=10), allocatable :: WGNAMES(:)
   character(len=10), allocatable :: UNITS(:)
   integer,           allocatable :: DATATIME(:)
   real*4,            allocatable :: PARAMS(:)

   open(13,file='prepobs.log')
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Read prep information from prepobs.in
   inquire(file='prepobs.def',exist=ex)
   if (.not.ex) stop 'prepobs: prepobs.def does not exist'
   open(10,file='prepobs.def')
      read(10,'(a)') measfile
      read(10,'(a4)')cmeastime
      read(cnext,'(i4.4)')meastime 
      write(13,*)'meastime= ',meastime
      read(10,*)
      m=0
      do
         m=m+1
         if (m <= maxobs) then
            read(10,'((a8,tr1,i2,tr1,a8,tr1,l1,tr1,e9.3,tr1,i3))',end=99,err=98) &
                  info(m)%wgname,info(m)%iw,info(m)%keywrd,info(m)%active,info(m)%std,info(m)%ierr
         else
            stop 'prepobs: maxobs needs to be increased'
         endif
         cycle
         99 nrobs=m-1
         write(13,*)'prepobs: expect to find ',nrobs,' measurements in observations.dat'
         exit
         98 stop 'prepobs: error in read from prepobs.in'
      enddo
   close(10)
 
   allocate(obs(nrobs))
   obs(:)%i=0
   obs(:)%j=0
   obs(:)%k=0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Read measurement file
   write(13,*)'prepobs: reads file      = ',trim(measfile)
   inquire(file=trim(measfile),exist=lmeas)
   if (.not.lmeas) stop 'prepobs: Eclipse measurement file does not exist'

   open(10,file=trim(measfile),form='formatted',access='sequential')

   imeas=1
   read(10,'(t3,a8,t13,i11,t26,a4)')measname(imeas),meassize(imeas),meastype(imeas)
   write(13,*)'HEADER: ',measname(imeas),meassize(imeas),meastype(imeas)
   if (measname(imeas) /= 'KEYWORDS') stop 'prepobs: KEYWORDS'
   allocate(KEYWORDS(meassize(imeas)))
   ird=read_char(meassize(imeas),KEYWORDS,meastype(imeas))

   imeas=2
   read(10,'(t3,a8,t13,i11,t26,a4)')measname(imeas),meassize(imeas),meastype(imeas)
   write(13,*)'HEADER: ',measname(imeas),meassize(imeas),meastype(imeas)
   if (measname(imeas) /= 'WGNAMES') stop 'prepobs: WGNAMES'
   allocate(WGNAMES(meassize(imeas)))
   ird=read_char(meassize(imeas),WGNAMES,meastype(imeas))

   imeas=3
   read(10,'(t3,a8,t13,i11,t26,a4)')measname(imeas),meassize(imeas),meastype(imeas)
   write(13,*)'HEADER: ',measname(imeas),meassize(imeas),meastype(imeas)
   if (measname(imeas) /= 'UNITS') stop 'prepobs: UNITS'
   allocate(UNITS(meassize(imeas)))
   ird=read_char(meassize(imeas),UNITS,meastype(imeas))

   do
      imeas=4
      read(10,'(t3,a8,t13,i11,t26,a4)',end=97,err=97)measname(imeas),meassize(imeas),meastype(imeas)
!         print *,'HEADER: ',measname(imeas),meassize(imeas),meastype(imeas)
      if (measname(imeas) /= 'DATATIME') stop 'prepobs: DATATIME'
      if (.not. allocated(DATATIME)) allocate(DATATIME(meassize(imeas)))
      ird=read_integer(meassize(imeas),DATATIME,meastype(imeas))
      
      imeas=5
      read(10,'(t3,a8,t13,i11,t26,a4)')measname(imeas),meassize(imeas),meastype(imeas)
!         print *,'HEADER: ',measname(imeas),meassize(imeas),meastype(imeas)
      if (measname(imeas) /= 'PARAMS') stop 'prepobs: PARAMS'
      if (.not. allocated(PARAMS)) allocate(PARAMS(meassize(imeas)))
      ird=read_real(meassize(imeas),PARAMS,meastype(imeas))

      if (DATATIME(1) == meastime ) exit
      
      cycle
      97 continue
      write(13,*)'Data for data time, ',meastime,', was not found'
      return
   enddo
   close(10)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Copies measurement data to obs variable
   iobs=0
   do m=1,nrobs
      if (.not.info(m)%active) cycle

      do i=3,meassize(5)
         if (info(m)%wgname == WGNAMES(i)(2:9).and.info(m)%keywrd == KEYWORDS(i)(2:9)) then
            iobs=iobs+1
            obs(iobs)%wgname=info(m)%wgname
            obs(iobs)%keywrd=info(m)%keywrd
            obs(iobs)%iw    =info(m)%iw
            obs(iobs)%d     =PARAMS(i)
            obs(iobs)%var=max( ((real(info(m)%ierr)/100.0)*obs(iobs)%d)**2, info(m)%std**2 )
            exit
         endif
      enddo
   enddo
   nnobs=iobs

   write(13,'(a)')' '
   write(13,'(a)')'prepobs: Cheking the well data: '
   do i=1,nnobs
      write(13,'(5i3,2(tr1,a8),2e13.3,a)')i,obs(i)
   enddo


   write(13,'(a)')' '
   write(13,'(a)')'prepobs: Removing well data where WOPRH is zero: '
   do i=1,nnobs
      if (obs(i)%d == 0.0 .and. trim(obs(i)%keywrd) == 'WOPRH') then
         iwell=obs(i)%iw
         do j=1,nnobs
            if (obs(j)%iw==iwell) then
               write(13,'(5i3,2(tr1,a8),2e13.3,a)')j,obs(j),' F'
               obs(j)%iw=-1
            endif   
         enddo
      endif
   enddo



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Marking zero measurements as false.
!   do i=1,nnobs
!      write(13,'(5i3,2(tr1,a8),2e13.3)',advance='no')i,obs(i)
!      if (obs(i)%d == 0.0) then
!         obs(i)%iw=-1
!         write(13,*)' F'
!      else
!         write(13,*)' T'
!      endif
!   enddo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Dumping observations.uf
   inquire(file='observations.uf',exist=ex)
   if (ex) call system('rm observations.uf')
   inquire(iolength=i)obs(1)
   open(10,file='observations.uf',form='unformatted',access='direct',recl=i)

   write(13,'(a)')' '
   write(13,*)'prepobs: Dumping active measurements to observations.uf'

   iobs=0
   do i=1,nnobs
      if (obs(i)%iw > 0) then
         iobs=iobs+1
         write(13,'(5i3,2(tr1,a8),2e13.3)')iobs,obs(i)
         write(10,rec=iobs)obs(i)
      endif
   enddo

   close(10)
   print '(a,/)','prepobs: done...'

end subroutine
end module
