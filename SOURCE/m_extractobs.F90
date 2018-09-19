module m_extractobs

contains
subroutine extractobs(cnext,day,nr)
! Reads data from different data sources and converts the
! information to the parameters stored in observations.uf
! Additional information is needed about the model grid.

   use mod_measurement
   use mod_dimensions
   use m_eclipse_read
   use mod_wellnames
   implicit none
   logical, save :: lfirst=.true.

   type obsinfo
      character(len=8) wgname    ! well name
      integer          iw        ! well number
      character(len=8) keywrd    ! name of observation variable
      logical          active    ! to be assimilated or not
      real             std       ! error standard dev 
      integer          ierr      ! error in % of data value
   end type obsinfo

   character(len=4),  intent(in) :: cnext
   real,              intent(in) :: day
   integer,           intent(in) :: nr

   character(len=80) :: varname
   character(len=4) ::  var(6)

   integer, parameter :: maxobs = 1000
   integer, parameter :: itot = 1000
   type (obsinfo),     allocatable ::  info(:)
   type (measurement), allocatable ::  obs(:)

   integer i,j,k,m,ird,ilen,nrobs,iobs,inactive,nnobs,itime,inext
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

   character(len=10), save, allocatable :: KEYWORDS(:)
   character(len=10), save, allocatable :: WGNAMES(:)
   character(len=10), save, allocatable :: UNITS(:)
   integer,           save, allocatable :: DATATIME(:,:)
   real*4,            save, allocatable :: PARAMS(:,:)


   nrobs=nrwells*6
   allocate(info(nrobs))
   allocate(obs(nrobs))

   open(13,file='extractobs.log')





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

   info(1:nrobs)%active=.true.
 
   obs(:)%i=0
   obs(:)%j=0
   obs(:)%k=0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   if (lfirst) then
   lfirst=.false.
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

   i=0
   do 
      i=i+1
      imeas=4
      read(10,'(t3,a8,t13,i11,t26,a4)',end=97,err=97)measname(imeas),meassize(imeas),meastype(imeas)
!         print *,'HEADER: ',measname(imeas),meassize(imeas),meastype(imeas)
      if (measname(imeas) /= 'DATATIME') stop 'prepobs: DATATIME'
      if (.not. allocated(DATATIME)) allocate(DATATIME(meassize(imeas),itot))
      ird=read_integer(meassize(imeas),DATATIME(:,i),meastype(imeas))

      
      imeas=5
      read(10,'(t3,a8,t13,i11,t26,a4)')measname(imeas),meassize(imeas),meastype(imeas)
!         print *,'HEADER: ',measname(imeas),meassize(imeas),meastype(imeas)
      if (measname(imeas) /= 'PARAMS') stop 'prepobs: PARAMS'
      if (.not. allocated(PARAMS)) allocate(PARAMS(meassize(imeas),itot))
      ird=read_real(meassize(imeas),PARAMS(:,i),meastype(imeas))

!      if (DATATIME(1,i) == meastime ) exit
      
      cycle
      97 continue
      exit
!      write(13,*)'Data for data time, ',meastime,', was not found'
!      return
   enddo
   close(10)

   endif

   read(cnext,'(i4.4)')inext
   itime=0
   do 
      itime=itime+1
      if (inext==DATATIME(1,itime)) exit
      if(itime> 1000) then
         print *,'No match in datatime: returning'
         return
      endif
   enddo


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Extracting measurement values
   m=0
   do i=1,nrwells
   do j=1,6
      m=m+1
      obs(m)%wgname=wells(i)
      obs(m)%keywrd=var(j)
      obs(m)%d=-999.999
      do k=3,meassize(5)
         if (info(m)%wgname == WGNAMES(k)(2:9).and.info(m)%keywrd == KEYWORDS(k)(2:9)) then
            obs(m)%d     =PARAMS(k,itime)
            print *,'match:+',info(m)%wgname,'+',wgnames(k)(2:9),'+',info(m)%keywrd,'+',keywords(k)(2:9),'+'
            exit
         endif
      enddo
!      if (k==meassize(5)+1) print *,'did not find observation value'
   enddo
   enddo




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Saving a tecplot file containing the history of all measurements

   var(1)='WBHP'
   var(2)='WOPR'
   var(3)='WGPR'
   var(4)='WWPR'
   var(5)='WWCT'
   var(6)='WGOR'


   write(13,*)'Saving measurement history.dat'
   inquire(file='Prodhist/history.dat',exist=ex)
   if (.not.ex) then
      open(10,file='Prodhist/history.dat',status='new',access='sequential')
      write(10,'(a)')'TITLE="History data"'
      write(10,'(a)',advance='no')'VARIABLES='
      write(10,'(5a)',advance='no')'"time"'

      do i=1,nrwells
         do j=1,6
            varname='"'//trim(wells(i))//':'//var(j)//'"'
            write(10,'(1x,a)',advance='no')trim(varname)
         enddo
      enddo
      write(10,'(a)')''

      write(10,'(a,i5,a)')'ZONE I=',nr,' F=POINT'
   else
      open(10,file='Prodhist/history.dat',status='old',access='sequential',position='append')
   endif
   write(10,'(f10.2)',advance='no')day


   do i=1,nrwells
      do j=1,6
         do m=1,nrobs
!           print *,'+',trim(wells(i)),'+',trim(info(m)%wgname),'+',trim(var(j)),'+',trim(info(m)%keywrd(1:4)),'+'
            if ( trim(wells(i)) == trim(info(m)%wgname)   .and. trim(var(j)) == trim(info(m)%keywrd(1:4)) ) then
                  write(10,'(1x,e12.5)',advance='no')obs(m)%d
            endif
         enddo
      enddo
   enddo
   write(10,'(a)')''
   close(10)
   return

   close(10)

!   deallocate(KEYWORDS,WGNAMES,UNITS,DATATIME,PARAMS)
   deallocate(info,obs)
   print *,'extractobs: done...'
end subroutine
end module
