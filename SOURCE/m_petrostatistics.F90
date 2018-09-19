module m_petrostatistics
   use mod_dimensions
   real, public, dimension(2,2,nz) :: avep, avex, avez ! Average poro and perm fields
   real, public, dimension(nz)     :: stdp, stdx, stdz ! Standard deviations for poro and perm
   real, public, dimension(nz)     :: rx,  ry, dir     ! Decorrelation lengths and direction
   real, public, dimension(nz)     :: pu, pl           ! Upper lower limits for poro
   real, public, dimension(nz)     :: xu, xl           ! Upper lower limits for permx
   real, public, dimension(nz)     :: zu, zl           ! Upper lower limits for permz

   real, public, dimension(nz)     :: Czz              ! Impose vertical correlations 
   real, public, dimension(nz)     :: Cpx              ! Impose poro-perm correlations
   real, public, dimension(nz)     :: Czx              ! Impose permx-permz correlations
   logical, public, dimension(nz)  :: active(nz)       ! Non-active uses original fields

   real, public :: principal_dir=90.0                  
contains
subroutine petrostatistics(logperm,returnflag)

   implicit none
   integer, parameter :: en=1
   logical ex,exA,exB,exC
   character(len=1) :: yn
   character(len=1) :: sep=''''
   character(len=1) :: sepp='"'
   logical, intent(in) :: logperm
   logical, intent(out) :: returnflag

   real aaa,bbb
   real, dimension(2,2) :: tmp
   integer k,i,j

   returnflag=.false.
   print *
   print '(a)','The file petrostatistics.dat must be '
   print '(a)','consistent with your geomodel definitions.'
   print *
   print '(a)','The simplest is to fill in values in the xls file'
   print '(a)','       petrostatistics.xls'
   print '(5a)','and store the sheet it in ',sep,'Text (Tab delimited) (*.txt)',sep,' format as'
   print '(a)','       petrostatistics.txt'
   print *

   inquire(file='petrostatistics.txt',exist=exA)
   inquire(file='petrostatistics.csv',exist=exC)
   inquire(file='petrostatistics.dat',exist=exB)

   if (.not.exA .and. .not.exC .and. .not.exB) then
      print *,'Neither petrostatistics.(txt,csv) or petrostatistics.dat exists!'
      returnflag=.true.
      return
   endif

   if (exA) then
      write(*,'(a)',advance='no')'Want to covert petrostatistics.txt to petrostatistics.dat (y/n): '
      read(*,*)yn
      if (yn == 'y') then
         open(10,file='petrostatistics.sh')
         write(10,'(a) ',advance='no')'cat petrostatistics.txt | '
         write(10,'(4a)',advance='no')'sed -e',sep,'1,2d',sep
         write(10,'(4a)',advance='no')' -e ',sep,'s/,/./g',sep
         write(10,'(4a)',advance='no')' -e ',sep,'s/T/.true./',sep
         write(10,'(4a)',advance='no')' -e ',sep,'s/F/.false./',sep
         write(10,'(a)') ' > petrostatistics.dat '
         close(10)
         call system('sh ./petrostatistics.sh')
      endif
   endif

   if (exC) then
      write(*,'(a)',advance='no')'Want to covert petrostatistics.csv to petrostatistics.dat (y/n): '
      read(*,*)yn
      if (yn == 'y') then
         open(10,file='petrostatistics.sh')
         write(10,'(a) ',advance='no')'cat petrostatistics.csv | '
         write(10,'(4a)',advance='no')'sed -e',sep,'1,2d',sep
         write(10,'(4a)',advance='no')' -e ',sep,'s/,/./g',sep
         write(10,'(4a)',advance='no')' -e ',sep,'s/T/.true./',sep
         write(10,'(4a)',advance='no')' -e ',sep,'s/F/.false./',sep
         write(10,'(a)') ' > petrostatistics.dat '
         close(10)
         call system('sh ./petrostatistics.sh')
      endif
   endif

   inquire(file='petrostatistics.dat',exist=exB)
   if (.not.exB) then
       print '(a)','Can not find petrostatistics.dat: returns'
       returnflag=.true.
       return
   endif

   open(10,file='petrostatistics.dat')
      do k=1,nz
         read(10,*)i,active(k),&
                   Czz(k),Cpx(k),Czx(k),&
                   avep(1,1,k),avep(2,1,k),avep(2,2,k),avep(1,2,k),&
                   avex(1,1,k),avex(2,1,k),avex(2,2,k),avex(1,2,k),&
                   avez(1,1,k),avez(2,1,k),avez(2,2,k),avez(1,2,k),&
                   stdp(k),stdx(k),stdz(k),&
                   rx(k),ry(k),dir(k),&
                   pl(k),pu(k),&
                   xl(k),xu(k),&
                   zl(k),zu(k)
         print '(i3,tr1,l1,7f5.2,13f8.1,f6.1,2f5.2,4f8.1)',i,active(k),&
                   Czz(k),Cpx(k),Czx(k),&
                   avep(1,1,k),avep(2,1,k),avep(2,2,k),avep(1,2,k),&
                   avex(1,1,k),avex(2,1,k),avex(2,2,k),avex(1,2,k),&
                   avez(1,1,k),avez(2,1,k),avez(2,2,k),avez(1,2,k),&
                   stdp(k),stdx(k),stdz(k),&
                   rx(k),ry(k),dir(k),&
                   pl(k),pu(k),&
                   xl(k),xu(k),&
                   zl(k),zu(k)
      enddo
   close(10)



   if (logperm) then
      print *
      print *,'NB: It is assumed that averages and std dev of permx and permz are'
      print *,'    given for the LOG_10(perm) distribution.  The truncation values'
      print *,'    are given for the simulated perm.'
      print *
      xl  =log10(xl)
      xu  =log10(xu)
      zl  =log10(zl)
      zu  =log10(zu)
   endif

! Correlation lengths and orientation.
! rx is the decorrelation length in the principal direction, ry is the
! decorrelation length orthogonal to the principal direction.  If the 
! decorrelation lengths are given corresponding to a spherical variogram
! they must coverted to Gaussian by scaling them with sqrt(3).  The
! direction is given relative to north and with clockwise rotation.
! This must be converted to mathematical units, relative to eastward
! and anticlockwise rotation.

! In the table the decorrelation lengths are given for a spherical variogram.
! Converting from Spherical to Gaussian
   rx=rx/sqrt(3.0)
   ry=ry/sqrt(3.0)

! In table the direction is given relative to north and rotating clockwise.
! Convertion to math angles with principal dir to the east and rotating 
! anticlockwise.  (e.g. 30 deg in table is 60 in math units).
   dir=principal_dir-dir

end subroutine
end module
