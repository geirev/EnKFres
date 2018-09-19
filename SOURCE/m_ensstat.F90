#undef PERMDIAG
module m_ensstat
contains
subroutine ensstat(iflag)
! computes enbsemble statistics
   use mod_states
   use mod_measurement
   use mod_localdefs
   use m_ensio
   use m_ensmean
   use m_ensvar
   use m_get_nrens
   use m_getmask
   use m_get_ensemble
   use m_eclipse_read
   use m_mksummary
   use m_tecfld
   use m_get_nrobs
   use m_get_obs
   use m_interface
#ifdef GAUSS2
   use mod_facies
#endif

   implicit none
   
   type(states4) mem4
   type(states), allocatable :: A(:),ref(:)
   type(states) ave,var,std
   logical ex,exref

   integer i,j,k,l,iflg,ifile,nrfiles,nens,ilen
   integer, parameter :: ndim=nx*ny*nz
   character(len=80) filename(1000)
   character(len=80) outfile
   character(len=250) command
   character(len=80) reffile
   character(len=9) rident
   character(len=4) cnum

   character(len=80) eclname
   character(len=80) ensname
   character(len=80) ensdir
   character(len=80) ecldir

   character(len=80) fname
   character(len=1) cvar

   type(measurement), allocatable :: obs(:)
   real res_perm,var_perm
   integer nact,nrobs
   integer iens,iopt,iflag

#ifdef GAUSS2
   real res_gauss1,res_gauss2,std_gauss1,std_gauss2
#endif

   integer reclA,isize
   real  ,  allocatable, dimension(:) :: rftpress(:,:)
   real*4,  allocatable, dimension(:) :: DEPTH,PRESSURE,SWAT,SGAS
   integer, allocatable, dimension(:) :: CONIPOS,CONJPOS,CONKPOS


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Read list of files to process
   inquire(file='ensstat.files',exist=ex)
   if (.not.ex) then
      print *,'ensstat: the file enstat.files does not exist'
      return
   endif
   open(10,file='ensstat.files')
      do i=1,1000
         filename(i)(:)=' '
         read(10,'(a)',end=98)filename(i)
      enddo
      98 nrfiles=i-1
   close(10)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Processing
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   do ifile=1,nrfiles

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading reference case information (from Refcase/refcaseF????.uf) if file exist

      ilen=len_trim(filename(ifile))
      cnum(1:4)=filename(ifile)(ilen-6:ilen-3)
      cvar(1:1)=filename(ifile)(ilen-7:ilen-7)
      reffile(:)=' '
      reffile(1:8)='Refcase/'
      reffile(9:16)='refcaseF'
      reffile(17:20)=cnum(1:4)
      reffile(21:23)='.uf'
      inquire(file=trim(reffile),exist=exref)
      if (exref) then
         allocate(ref(1))
         iflg=open_ensemblefile(trim(reffile),'old')
         iflg=read_ensmem(1,mem4)
         ref(1)=mem4
         close(10)
      endif


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading ensemble files and computing statistics
      print '(a,a)','ensstat: processing file: ',trim(filename(ifile))
      allocate(A(nrens))
      call get_ensemble(trim(filename(ifile)),A)
      call ensmean(A,ave)
      call ensvar(A,ave,var)
      std=sqrt(var)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Some special diagnostics related to the use of plurigaussian fields
! Computes diagnostic poro and perm fields from average of plurigaussian fields
#ifdef GAUSS2
      gauss1=ave%gauss1
      gauss2=ave%gauss2

      call facies_def
      call facies_compute

      ave%poro=poro3d
      ave%permx=10**(9.02*ave%poro+0.77)
      ave%permz=0.31*ave%permx + 3.12
#endif


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Permeability dioagnostics (not normally used)
#ifdef PERMDIAG
      if (ifile==1 .and. exref) then
         call permdiag(ref,A,nens)
      endif
#endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      deallocate(A)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Output Eclipse summary file
      if (cnum /= '0000') then
         call mksummary(ave,cvar,cnum,'AVE')
         call mksummary(std,cvar,cnum,'STD')
      endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Output ensemble average, variance and standard deviation

      outfile(:)=' '
      outfile='Ensstat/ensave'//cvar//cnum//'.uf'
      print '(2a)','Saving ensemble average file: ',trim(outfile)
      iflg=open_ensemblefile(trim(outfile),'unknown')
         rident='AVERAGE  '
         mem4=ave
         iflg=write_ensmem(1,rident,mem4)
      close(10)

      outfile='Ensstat/ensstd'//cvar//cnum//'.uf'
      print '(2a)','Saving ensemble std dev file: ',trim(outfile)
      iflg=open_ensemblefile(trim(outfile),'unknown')
         rident='STDDEV   '
         mem4=std
         iflg=write_ensmem(1,rident,mem4)
      close(10)

      command(:)=' '
      command='cd Ensstat; ln -sf ../'//trim(ensbase)//'S'//cnum//'.uf '//'ensaveS'//cnum//'.uf'
      print '(2a)','Executing command: ',trim(command)
      call system(trim(command))

      command='cd Ensstat; ln -sf ../'//trim(ensbase)//'S'//cnum//'.uf '//'ensstdS'//cnum//'.uf'
      print '(2a)','Executing command: ',trim(command)
      call system(trim(command))


! Only save Diag restart files when iflag=1
      if (iflag==1)  then
         ensdir='Ensstat/'
         ensname='ensave'
         ecldir='Diag/'
         eclname='AVE_'//cvar
         iens=1
         iopt=22
         call interface(ensdir, ensname, ecldir, eclname, cvar,cnum,iens,iopt)

         command(:)=' '
         command='gzip -f Diag/AVE_A.F'//cnum
         call system(trim(command))

         command(:)=' '
         command='cd Diag; ln -sf ../'//trim(refdir)//trim(eclbase)//'.FGRID  '//trim(eclname)//'.FGRID'
         call system(trim(command))

         command(:)=' '
         command='cd Diag; ln -sf ../'//trim(refdir)//trim(eclbase)//'.FINIT  '//trim(eclname)//'.FINIT'
         call system(trim(command))


         ensdir='Ensstat/'
         ensname='ensstd'
         ecldir='Diag/'
         eclname='STD_'//cvar
         iens=1
         iopt=22
         call interface(ensdir, ensname, ecldir, eclname, cvar,cnum,iens,iopt)

         command(:)=' '
         command='gzip -f Diag/STD_A.F'//cnum
         call system(trim(command))

         command(:)=' '
         command='cd Diag; ln -sf ../'//trim(refdir)//trim(eclbase)//'.FGRID  '//trim(eclname)//'.FGRID'
         call system(trim(command))

         command(:)=' '
         command='cd Diag; ln -sf ../'//trim(refdir)//trim(eclbase)//'.FINIT '//trim(eclname)//'.FINIT'
         call system(trim(command))
      endif


#ifdef MULTZ
      open(10,file='multz_ave.dat',position='append')
         write(10,'(a13,100f13.5)')cnum,ave%multz(1:nz-1)
      close(10)
#endif

#ifdef MULTFLT
      open(10,file='multflt_ave.dat',position='append')
         write(10,'(a13,100f13.5)')cnum,ave%multflt(1:nrflts)
      close(10)
#endif

#ifdef EQUIL
      open(10,file='equilWOC_ave.dat',position='append')
         write(10,'(a13,100f13.5)')cnum,ave%equilWOC(1:eqldims)
      close(10)
      open(10,file='equilGOC_ave.dat',position='append')
         write(10,'(a13,100f13.5)')cnum,ave%equilGOC(1:eqldims)
      close(10)
#endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Output permeability residual (if Reffile exists)
      if (exref) then
#ifdef GAUSS2
         res_gauss1=0.0
         res_gauss2=0.0
         std_gauss1=0.0
         std_gauss2=0.0
         do k=1,nz
         do j=1,ny
         do i=1,nx
             res_gauss1=res_gauss1+real(mask(i,j,k))*(ave%gauss1(i,j,k)-ref(1)%gauss1(i,j,k))**2
             res_gauss2=res_gauss2+real(mask(i,j,k))*(ave%gauss2(i,j,k)-ref(1)%gauss2(i,j,k))**2
             std_gauss1=std_gauss1+real(mask(i,j,k))*var%gauss1(i,j,k)
             std_gauss2=std_gauss2+real(mask(i,j,k))*var%gauss2(i,j,k)
         enddo
         enddo
         enddo
         
         nact=sum(mask(:,:,:))
         res_gauss1=sqrt(res_gauss1/real(nact))
         res_gauss2=sqrt(res_gauss2/real(nact))
         std_gauss1=sqrt(std_gauss1/real(nact))
         std_gauss2=sqrt(std_gauss2/real(nact))

         inquire(file='gaussres.tmp',exist=ex)
         if (ex) then
            open(11,file='gaussres.tmp',access='sequential',position='append')
               write(11,'(a,4f12.2)')cnum,res_gauss1,res_gauss2,std_gauss1,std_gauss2
            close(11)
         else
            open(11,file='gaussres.tmp',access='sequential',position='append')
               write(11,'(a)')'TITLE="Residual of Gaussian fields"'
               write(11,'(a)')'VARIABLES= "time" "resG1" "resG2" "stdG1" "stdG2"'
               write(11,'(a)')'ZONE I=XXXX F=POINT'
               write(11,'(a,4f12.2)')cnum,res_gauss1,res_gauss2,std_gauss1,std_gauss2
            close(11)
         endif

#else
!         open(10,file='reffile1.dat')
!         do j=1,ny
!            write(10,'(i3,f10.2)')j,ref(1)%permx(nx/2,j,1)
!         enddo
!         close(10)

!         open(10,file='solfile2.dat')
!         do j=1,ny
!            write(10,'(i3,2f10.2)')j,ave%permx(nx/2,j,1),sqrt(var%permx(nx/2,j,1))
!         enddo
!         close(10)

!         res_perm=0.0
!         var_perm=0.0
!         do k=1,nz
!         do j=1,ny
!         do i=1,nx
!            res_perm=res_perm+real(mask(i,j,k))*(ave%permx(i,j,k)-ref(1)%permx(i,j,k))**2
!            var_perm=var_perm+real(mask(i,j,k))*var%permx(i,j,k)
!         enddo
!         enddo
!         enddo
!         nact=sum(mask(:,:,:))
!         print *,'ensstat: nact, ndim: ',nact,ndim
!         open(11,file='permres.tmp',access='sequential',position='append')
!            write(11,'(a,2f12.2)')trim(filename(ifile)), sqrt(res_perm/real(nact)),&
!	       sqrt(var_perm/real(nact))
!         close(11)
#endif

      endif
      if (allocated(ref)) deallocate(ref)

      filename(i)(:)=' '
      filename(i)(1:13)='ensrft'//cnum(1:4)//'.uf'
      print *,'ensref file: ',trim(filename(i))
      inquire(file=trim(filename(i)),exist=ex)
      if (ex) then
         print *,'Ensreffile exists'
         inquire(iolength=reclA)isize
         open(10,file=trim(filename(i)),form='unformatted',access='direct',recl=reclA)
            read(10,rec=1)isize
         close(10)
         print *,'isize=',isize
         allocate(conipos(isize),conjpos(isize),conkpos(isize),depth(isize),pressure(isize),swat(isize),sgas(isize))
         allocate(rftpress(isize,nrens))
         inquire(iolength=reclA)isize,CONIPOS,CONJPOS,CONKPOS,DEPTH,PRESSURE,SWAT,SGAS
         open(10,file=trim(filename(i)),form='unformatted',access='direct',recl=reclA)
            do iens=1,100
               read(10,rec=iens)isize,CONIPOS,CONJPOS,CONKPOS,DEPTH,PRESSURE,SWAT,SGAS
               rftpress(:,iens)=pressure(:)
            enddo
         close(10)

         open(10,file='ensrft'//cnum(1:4)//'.dat')
            do iens=1,100
               if (iens==1) write(10,'(a)')'VARIABLES = "Depth", "Pressure"'
               write(10,'(a,i5,a,i4,a)')'ZONE I=',isize,', T = "',iens,'", DATAPACKING=BLOCK'
               write(10,'(5g13.5)')depth(:)
               write(10,'(5g13.5)')rftpress(:,iens)
            enddo
         close(10)


         deallocate(conipos,conjpos,conkpos,depth,pressure,swat,sgas)
         deallocate(rftpress)
      endif
      

   enddo

   print *,'enstat: done....'
end subroutine
end module

