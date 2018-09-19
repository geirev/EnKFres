module  m_poroobs
contains
subroutine poroobs()

! Reads data from different data sources and converts the
! information to the parameters stored in observations.uf
! Additional information is needed about the model grid.

   use mod_localdefs
   use mod_measurement
   use m_getmask
   implicit none


   integer, parameter :: maxobs = 10000
   type (measurement), allocatable ::    obs(:)
   integer nrobs,m,i,j,k,iopt,iobs,nractive,nrlines
   logical ex
   real dummy,varporo,varperm,poro,perm

! Read poro data from poroobs.dat
   inquire(file='poroobs.dat',exist=ex)
   if (.not.ex) stop 'poroobs: poroobs.dat does not exist'
   open(10,file='poroobs.dat')
      nractive=0
      do m=1,maxobs
         read(10,*,end=999)i,j,k
         if (lmask(i,j,k)) nractive=nractive+1
      enddo
 999  nrlines=m-1
   close(10)
   print '(a,i4)','number of lines in poroobs.dat file is= ',nrlines
   print '(a,i4)','number of active lines in poroobs.dat file is= ',nractive

   write(*,'(a)')'Standard file format is: '
   write(*,'(a)')'i-index, j-index, k-index, PORO, PERM'
   write(*,'(a)')'Conditioning on PORO and PERM: 1'
   write(*,'(a)')'Conditioning only on PORO    : 2'
   write(*,'(a)')'Conditioning only on PERM    : 3'
   write(*,'(a)',advance='no')'Chose conditioning variables: '
   read(*,*)iopt

   if (iopt < 3) then
      write(*,'(a)',advance='no')'Chose error standeard deviation of porosity data (in absolute value): '
      read(*,*)varporo
      varporo=varporo**2
   endif
   if (iopt /= 2) then
      if (logperm) then
         write(*,'(a)',advance='no')'Chose error standard deviation of log(permeability) data (in absolute value): '
      else
         write(*,'(a)',advance='no')'Chose error standard deviation of permeability data (in absolute value): '
      endif
      read(*,*)varperm
      varperm=varperm**2
   endif

   if (iopt == 1) then
      nrobs=2*nractive
   else
      nrobs=nractive
   endif

   allocate(obs(nrobs))
   open(10,file='poroobs.dat')
      iobs=0
      do m=1,nrlines
         read(10,*)i,j,k,poro,perm
         if (lmask(i,j,k)) then
            if (logperm)perm=log(1.0+perm)
            if (iopt == 1) then
               iobs=iobs+1
               obs(iobs)%i=i
               obs(iobs)%j=j
               obs(iobs)%k=k
               obs(iobs)%d=poro
               obs(iobs)%var=varporo
               obs(iobs)%keywrd(1:8)='PORO    '
               obs(iobs)%iw=0
               obs(iobs)%wgname(1:8)='        '

               iobs=iobs+1
               obs(iobs)%i=i
               obs(iobs)%j=j
               obs(iobs)%k=k
               obs(iobs)%d=perm
               obs(iobs)%var=varperm
               obs(iobs)%keywrd(1:8)='PERM    '
               obs(iobs)%iw=0
               obs(iobs)%wgname(1:8)='        '
            endif

            if (iopt == 2) then
               iobs=iobs+1
               obs(iobs)%i=i
               obs(iobs)%j=j
               obs(iobs)%k=k
               obs(iobs)%d=poro
               obs(iobs)%var=varporo
               obs(iobs)%keywrd(1:8)='PORO    '
               obs(iobs)%iw=0
               obs(iobs)%wgname(1:8)='        '
            endif

            if (iopt == 3) then
               iobs=iobs+1
               obs(iobs)%i=i
               obs(iobs)%j=j
               obs(iobs)%k=k
               obs(iobs)%d=perm
               obs(iobs)%var=varperm
               obs(iobs)%keywrd(1:8)='PERM    '
               obs(iobs)%iw=0
               obs(iobs)%wgname(1:8)='        '
            endif

         endif

!        read(10,*)obs(m)%i,obs(m)%j,obs(m)%k,obs(m)%d
!        read(10,'(3i4,f11.6)')obs(m)%i,obs(m)%j,obs(m)%k,obs(m)%d
!        print '(3i4,f11.6,i4)',obs(m)%i,obs(m)%j,obs(m)%k,obs(m)%d,m
      enddo
   close(10)


   inquire(file='observations.uf',exist=ex)
   if (ex) call system('rm observations.uf')
   inquire(iolength=m)obs(1)
   open(10,file='observations.uf',form='unformatted',access='direct',recl=m)
   print '(a)','prepobs: Dumping poro measurements to observations.uf'
   do m=1,nrobs
      print '(4i3,i3,2(tr1,a8),2e13.3)',m,obs(m)
      write(10,rec=m)obs(m)
   enddo
   close(10)
   print*,'poroobs: done...'

end subroutine
end module
