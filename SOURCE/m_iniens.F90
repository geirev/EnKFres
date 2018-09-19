module m_iniens
contains
subroutine iniens(ensfile,nens,iflag)
! generates the initial ensemble for  EnKF Eclipse 
!
!  iflag=1:  creates an ensemble of pseudo random perturbations for
!            poro and perm as defined in m_statistics.F90 and stores
!            an ensemble of model states ready for use in enkf.
!  iflag=2:  Creates a random prior PORO.INC, PERMX.INC and PERMZ.INC
!            and MULTFLT.INC, MULTZ.INC, and EQUILS.INC
!  iflag=3:  Reads the prior PORO.INC, PERMX.INC and PERMZ.INC and computes
!            statistics
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use mod_dimensions
   use mod_states
   use mod_localdefs
   use m_gridextent
   use m_sample2D
   use m_ensio
   use m_random
   use m_random2
   use m_ensvar
   use m_getmask
   use m_readorig
   use m_tecfld
   use m_set_random_seed2
   use m_bilin2
   use m_eclipse_read

   use m_petrostatistics
#ifdef MULTFLT
   use m_inimultflt
#endif
#ifdef MULTZ
   use m_inimultz
#endif
#ifdef EQUIL
   use m_iniequil
#endif

   implicit none

   character(len=*), intent(in)    :: ensfile
   integer,          intent(in)    :: nens
   integer,          intent(in)    :: iflag
   integer, parameter :: en=1


! Local variables
   character(len=1) :: sep=''''

   type(states) mem
   type(states4) mem4

   real, allocatable, dimension(:,:,:,:) :: sampx,sampz,sampp

   character(len=9), parameter :: cident='INIENS   ' ! Ensemble file version

   integer num
   logical returnflag

   real, dimension(nx,ny,nz) :: newpermx, newpermz, newporo

   integer iens,i,j,k,k1,m,n,n1,n2
   integer reclA     ! ensemble record length
   integer reclB     ! ensemble record length
   logical ex
   integer iflg
   real alpha             ! Vertical correlation parameter alpha^(i-j)
   real, parameter :: pi=3.1415927

   character(len=1)yn
   character(len=80)fname,command
   real, allocatable :: Amat(:,:,:)
   integer, allocatable :: amask(:,:)
   integer, parameter :: ndim=nx*ny*nz
   real aa,bb,a1,a2,a3,a4,tmp

      integer iactive
      real tmpp(nactive)
   print *,'iflag=', iflag

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   if (iflag == 1 .or. iflag == 2) then
! Defining statistics for poro and perm in all layers
      call petrostatistics(logperm,returnflag)
      if (returnflag) return

#ifdef MULTZ
      call inimultz(iflag,returnflag)
      if (returnflag) return
#endif

#ifdef MULTFLT
      call inimultflt(iflag,returnflag)
      if (returnflag) return
#endif

#ifdef EQUIL
      call iniequil(iflag,returnflag)
      if (returnflag) return
#endif
   endif



   if (iflag == 1) then
      print *
      if (nre == 1) then
         print '(a)','iniens is running without improved sampling: nre=1'
      endif   
      write(*,'(a,i2,a)',advance='no')'Do you want to continue with nre=',nre,' (y/n): '
      read(*,*)yn
      if (yn /= 'y') then
         print *
         print '(a)','Note that nre > 2-4 may lead to slow (overnight) initialization.'
         write(*,'(a)',advance='no')'Please give new value for nre (nre>6 returns to main menu): '
         read(*,*)nre
      endif

   elseif (iflag == 2) then
      nre=1
   endif

   if (iflag == 1) then
      if (nre > 6) then
         print *
         print '(a)','WARNING: nre is set larger than 6 and I return.'
         print *
         return
      endif
   endif



   print *,'mask in layer one'
   do j=ny,1,-1
      write(*,'(a)',advance='no')'|'
      do i=1,nx
         write(*,'(i1)',advance='no')mask(i,j,1)
      enddo
      write(*,*)'|'
   enddo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Read original PORO and PERM and compute pdfs and averages
   if (iflag == 1 .or. iflag == 3) then
      print *,'----------------------------------------------------------'
      print *,'iniens: reading and diagnosing original PORO and PERM'
      call readorig(mask)
      if (iflag == 3) return
   endif


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Checking for inactive layers
   do k=1,nz
      if (active(k)) then
         if (sum(mask(:,:,k)) == 0) then
            active(k)=.false.
            print '(a,i3,a)','Layer ',k,' has no active cells and is set to inactive.'
         endif
      endif
   enddo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   print *,'----------------------------------------------------------'
   print *,'iniens: computing grid extent'
   fname(:)=' '
   i=len_trim(refpath)
   fname(1:i)=trim(refpath)
   j=len_trim(eclbase)
   fname(i+1:i+j)=trim(eclbase)
   fname(i+j+1:i+j+6)='.FGRID'
   print *,'---',trim(fname)
   inquire(file=trim(fname),exist=ex)
   if (.not. ex) then
      print *
      print *,'WARNING: Refcase/ECLIPSE.FGRID not found.'
      print *,'Run reference case first.'
      print *,'Returning from iniens without generation of prior or initial ensemble.'
      print *
      return
   endif
   call gridextent(lmask)


   print '(a,2f12.2)','Position (1 , 1)=', xpos( 1, 1,1),ypos( 1, 1,1)
   print '(a,2f12.2)','Position (nx, 1)=', xpos(nx, 1,1),ypos(nx, 1,1)
   print '(a,2f12.2)','Position (nx,ny)=', xpos(nx,ny,1),ypos(nx,ny,1)
   print '(a,2f12.2)','Position ( 1,ny)=', xpos( 1,ny,1),ypos( 1,ny,1)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Set a variable random seed
   print *,'----------------------------------------------------------'
   print *,'iniens: set random seed'
   call set_random_seed2


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! read first guess model state (with permeability) from refcase initial state
   if (iflag==1) then
      print *,'----------------------------------------------------------'
      print *,'iniens: reading first guess ensemble file'
      fname(:)=' '
      i=len_trim(refpath)
      fname(1:i)=trim(refpath)
      fname(i+1:i+15)='refcaseF0000.uf'
      iflg=open_ensemblefile(trim(fname),'old')
      iflg=read_ensmem(1,mem4)
      close(10)
      mem=mem4
   endif

   mem%permx=0.0
   mem%permz=0.0
   mem%poro=0.0



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Sample all random fields and interpolate to model grid
   allocate(Amat(mx,my,nens))
   allocate(amask(mx,my))
   amask=1
   allocate(sampx(nx,ny,nz,nens)); sampx=0.0
   allocate(sampz(nx,ny,nz,nens)); sampz=0.0
   allocate(sampp(nx,ny,nz,nens)); sampp=0.0

!!!$OMP PARALLEL DO PRIVATE(k,n,Amat) SHARED(mx,my,nens,nre,dx,dy,minxpos,minypos)
!!!$OMP&         SCHEDULE(STATIC,1)
   do k=1,nz
      if (active(k)) then
         print *,'Generating samples for layer: ',k

         if (stdp(k) > 0.0 ) then
            print *,'sampp',k
            call sample2D(Amat,mx,my,nens,nre,dx,dy,rx(k),ry(k),dir(k),.true.)
            do n=1,nens
               call bilin2(Amat(:,:,n),mx,my,minxpos,minypos,dx,dy,sampp(:,:,k,n),nx,ny,xpos(:,:,k),ypos(:,:,k),lmask(:,:,k))
            enddo
            if (k==1) then
               call tecfld2('bilinA',mx,my,1,Amat(:,:,1),amask,minxpos,minypos,dx,dy)
               call tecfld3('bilinB',nx,ny,1,sampp(:,:,1,1),mask,xpos,ypos)
            endif
         endif

         if (stdx(k) > 0.0 ) then
            print *,'sampx',k
            call sample2D(Amat,mx,my,nens,nre,dx,dy,rx(k),ry(k),dir(k),.true.)
            do n=1,nens
               call bilin2(Amat(:,:,n),mx,my,minxpos,minypos,dx,dy,sampx(:,:,k,n),nx,ny,xpos(:,:,k),ypos(:,:,k),lmask(:,:,k))
            enddo
         endif

         if (stdz(k) > 0.0 ) then
            print *,'sampz',k
            call sample2D(Amat,mx,my,nens,nre,dx,dy,rx(k),ry(k),dir(k),.true.)
            do n=1,nens
               call bilin2(Amat(:,:,n),mx,my,minxpos,minypos,dx,dy,sampz(:,:,k,n),nx,ny,xpos(:,:,k),ypos(:,:,k),lmask(:,:,k))
            enddo
         endif
      endif
   enddo
   print *,'samples done'


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Impose statistics
   do iens=1,nens
      newpermx(:,:,k)= -7.0
      newpermz(:,:,k)= -8.0
      newporo(:,:,k) = -9.0
! vertical correlations
      do k=2,nz
         if (active(k)) then
            alpha=czz(k)
            sampx(:,:,k,iens)=alpha*sampx(:,:,k-1,iens)+sqrt(1.0-alpha**2)*sampx(:,:,k,iens)
            sampp(:,:,k,iens)=alpha*sampp(:,:,k-1,iens)+sqrt(1.0-alpha**2)*sampp(:,:,k,iens)
            sampz(:,:,k,iens)=alpha*sampz(:,:,k-1,iens)+sqrt(1.0-alpha**2)*sampz(:,:,k,iens)
         endif
      enddo

! poro-permx correlations
      do k=1,nz
         if (active(k)) then
            alpha=cpx(k)
            sampx(:,:,k,iens)=alpha*sampp(:,:,k,iens)+sqrt(1.0-alpha**2)*sampx(:,:,k,iens)
         endif
      enddo

! permz-permx correlations
      do k=1,nz
         if (active(k)) then
            alpha=czx(k)
            sampz(:,:,k,iens)=alpha*sampx(:,:,k,iens)+sqrt(1.0-alpha**2)*sampz(:,:,k,iens)
         endif
      enddo

! add mean and scale to correct variance
      do k=1,nz
         if (active(k)) then
            do j=1,ny
            do i=1,nx
               aa=(xpos(i,j,k) - xpos(1,1,k))/(xpos(nx,1,k) - xpos(1,1,k))
               bb=(ypos(i,j,k) - ypos(1,1,k))/(ypos(1,ny,k) - ypos(1,1,k))

               a1=(1.0-aa)*(1.0-bb)
               a2=aa*(1.0-bb)
               a3=aa*bb
               a4=(1.0-aa)*bb

               tmp = a1*avep(1,1,k)+a2*avep(2,1,k)+a3*avep(2,2,k)+a4*avep(1,2,k)
               sampp(i,j,k,iens)= tmp + stdp(k) * sampp(i,j,k,iens)

               tmp = a1*avex(1,1,k)+a2*avex(2,1,k)+a3*avex(2,2,k)+a4*avex(1,2,k)
               sampx(i,j,k,iens)=tmp + stdx(k) * sampx(i,j,k,iens)

               tmp = a1*avez(1,1,k)+a2*avez(2,1,k)+a3*avez(2,2,k)+a4*avez(1,2,k)
               sampz(i,j,k,iens)=tmp + stdz(k) * sampz(i,j,k,iens)
            enddo
            enddo
         endif
      enddo


! impose cutoffs 
      do k=1,nz
         if (active(k)) then
            if (pl(k)  == 0.0) then
               print *,'Poro lower cutoff in layer',k,pl(k)
               pl(k)=0.00001
               print *,'reset to : ',pl(k)
               print *,'check number of decimals stored in petrophysics.xls'
            endif
            sampx(:,:,k,iens)=max(min(sampx(:,:,k,iens),xu(k)),xl(k))
            sampz(:,:,k,iens)=max(min(sampz(:,:,k,iens),zu(k)),zl(k))
            sampp(:,:,k,iens)=max(min(sampp(:,:,k,iens),pu(k)),pl(k))
         endif
      enddo

! reintitialize a model state
      where (lmask)
         newpermx(:,:,:)=sampx(:,:,:,iens)
         newpermz(:,:,:)=sampz(:,:,:,iens)
         newporo (:,:,:)=sampp(:,:,:,iens)
      endwhere

! Working on log(perm), rather than perm
      if (logperm) then
         where (lmask)
            newpermx=10**(newpermx)
            newpermz=10**(newpermz)
         endwhere
      endif

! quick check of perm and poro values
      if (iens < 30) then
         print *
         do k=1,nz
            if (active(k)) then
               print '(a,2i3,2(a,f8.3,a,f9.3),a,f5.3,a,f6.3,a)','petrophysics min/max: ',iens,k,&
                  '    permx(',minval(newpermx(:,:,k),MASK=lmask(:,:,k)),' -',&
                               maxval(newpermx(:,:,k),MASK=lmask(:,:,k)),&
                  ');  permz(',minval(newpermz(:,:,k),MASK=lmask(:,:,k)),' -',&
                               maxval(newpermz(:,:,k),MASK=lmask(:,:,k)),&
                  ');   poro(',minval(newporo(:,:,k),MASK=lmask(:,:,k)),' -', &
                               maxval(newporo(:,:,k),MASK=lmask(:,:,k)),')'
            endif
         enddo
      endif


! Restore to original BC values in not active layers
      do k=1,nz
         if (.not.active(k)) then
            print *,'WARNING: restores to original poro-perm in layer: ',k
            newpermx(:,:,k)= permxo(:,:,k)
            newpermz(:,:,k)= permzo(:,:,k)
            newporo(:,:,k) = poroo(:,:,k)
         endif
      enddo


      if (iens == 1) then
         call tecfld3('poro',nx,ny,nz,newporo,mask,xpos,ypos)
         call tecfld3('permX',nx,ny,nz,newpermx,mask,xpos,ypos)
         call tecfld3('permZ',nx,ny,nz,newpermz,mask,xpos,ypos)
      endif

! copy to mem in packed form
      mem=0.0
      mem%permx=pack(newpermx,lmask)
      mem%permz=pack(newpermz,lmask)
      mem%poro =pack(newporo,lmask)

!      iactive=0
!      do k=1,nz
!      do j=1,ny
!      do i=1,nx
!         if (lmask(i,j,k)) then
!            iactive=iactive+1
!            tmpp(iactive)=newporo(i,j,k)
!            if (tmpp(iactive)== 0.0) then
!               print '(f10.3,i7,3i5)',tmpp(iactive),iactive,i,j,k
!            endif
!         endif
!      enddo
!      enddo
!      enddo
!      print *,'iactive nactive :',iactive,nactive
!
!      print *,'Iniens:'
!      open(12,file='pp1.dat')
!         do i=1,nactive
!            write(12,'(2g13.5,i7)')mem%poro(i),tmpp(i),i
!         enddo
!      close(12)
!      stop


#ifdef MULTFLT
      call random(mem%multflt,nrflts)
      do i=1,nrflts
         mem4%multflt(i)=max(0.0, faultmean(i)+faultstd(i)*mem%multflt(i))
      enddo
#endif

#ifdef MULTZ
      call random(mem%multz,nz-1)
      do i=2,nz
         mem4%multz(i)=max(0.0, multzmean(i)+multzstd(i)*mem%multz(i))
      enddo
#endif

#ifdef EQUIL
      call random2(mem%equilWOC,eqldims)
      call random2(mem%equilGOC,eqldims)
      do i=1,eqldims
         mem4%equilWOC(i)=max(0.0, eqwoc(i)+eqwocstd(i)*mem%equilWOC(i))
         mem4%equilGOC(i)=max(0.0, eqgoc(i)+eqgocstd(i)*mem%equilGOC(i))
         mem4%equilGOC(i)=min(mem4%equilGOC(i), mem4%equilWOC(i) )
      enddo
#endif

! save to ensemble file
      if (iflag==1) then
         iflg=open_ensemblefile(trim(ensfile),'unknown')
            print *,'maxval: ',iens,maxval(mem%permx),maxloc(mem%permx)
            mem4%permx=mem%permx
            mem4%permz=mem%permz
            mem4%poro=mem%poro

#ifdef EQUIL
      do i=1,eqldims
         print *,'equil: ', mem4%equilWOC(i),mem4%equilGOC(i)
      enddo
#endif
            iflg=write_ensmem(iens,cident,mem4)
         close(10)

!         inquire(iolength=reclA)iens,mem4%poro,mem4%permx,mem4%permz
!         open(10,file='poroperm.uf',form='unformatted',access='direct',recl=reclA)
!            write(10,rec=iens)iens,mem4%poro,mem4%permx,mem4%permz
!         close(10)
      endif

   enddo


   if (iflag == 2) then
      fname(:)=' '
      i=len_trim(pridir)
      fname(1:i)=trim(pridir)
      inquire(file=fname(1:i),exist=ex)
      if (.not.ex) then
         command(:)=' '
         command(1:i+6)='mkdir '//fname(1:i) 
         call system(command(1:i+6))
      endif

      fname(i+1:i+8)='PORO.INC'
      print *,'Saves random prior ',trim(fname)
      open(10,file=trim(fname))
         write(10,'(a)')'PORO'
         newporo=0.0
         newporo=unpack(mem%poro,lmask,newporo)
         write(10,'(5(e14.6))')newporo
         write(10,'(a)')' /'
      close(10)

      fname(i+1:i+9)='PERMX.INC'
      print *,'Saves random prior ',trim(fname)
      open(10,file=trim(fname))
         write(10,'(a)')'PERMX'
         newpermx=0.0
         newpermx=unpack(mem%permx,lmask,newpermx)
         write(10,'(5(e14.6))')newpermx
         write(10,'(a)')' /'
      close(10)

      fname(i+1:i+9)='PERMZ.INC'
      print *,'Saves random prior ',trim(fname)
      open(10,file=trim(fname))
         write(10,'(a)')'PERMZ'
         newpermz=0.0
         newpermz=unpack(mem%permz,lmask,newpermz)
         write(10,'(5(e14.6))')newpermz
         write(10,'(a)')' /'
      close(10)

#ifdef MULTZ
      fname(i+1:i+9)='MULTZ.INC'
      print *,'Saves random prior ',trim(fname)
      open(10,file=trim(fname))
         do k=1,nz-1
            write(10,'(a)')'BOX'
            write(10,'(6i5,a)')en, nx, en, ny, k, k,' /'
            write(10,'(a)')'MULTZ'
            select case (nx*ny)
            case(10:99)
               write(10,'(i2,a,E11.5,a)')nx*ny,'*',mem4%multz(k),' /'
            case(100:999)
               write(10,'(i3,a,E11.5,a)')nx*ny,'*',mem4%multz(k),' /'
            case(1000:9999)
               write(10,'(i4,a,E11.5,a)')nx*ny,'*',mem4%multz(k),' /'
            case(10000:99999)
               write(10,'(i5,a,E11.5,a)')nx*ny,'*',mem4%multz(k),' /'
            case(100000:999999)
               write(10,'(i6,a,E11.5,a)')nx*ny,'*',mem4%multz(k),' /'
            end select
            write(10,'(a)')'ENDBOX'
            write(10,'(a)')' '
         enddo
      close(10)
#endif

#ifdef EQUIL
      fname(i+1:i+9)='EQUIL.INC'
      print *,'Saves random prior ',trim(fname)
      open(10,file='equil.dat')
      open(11,file=trim(fname))
         write(11,'(a)')'EQUIL'
         write(11,'(a)')'-- depth       P     WOC      Pc     GOC      Pc  Rs(d) Rv(d) Acc(IIP)'
         do i=1,eqldims
            read(10,*)eqdepth(i), eqp(i), eqwoc(i), eqpc1(i), eqgoc(i),&
                              eqpc2(i), eqrs(i), eqrv(i), eqacc(i), eqwocstd(i), eqgocstd(i)
            write(11,'(6f8.2,3i5,a)')   &
                              eqdepth(i), eqp(i), mem4%equilWOC(i), eqpc1(i), mem4%equilGOC(i),&
                              eqpc2(i), eqrs(i), eqrv(i), eqacc(i),'/'
         enddo
      close(10)
      close(11)
#endif

#ifdef MULTFLT
      fname(i+1:i+11)='MULTFLT.INC'
      print *,'Saves random prior ',trim(fname)
      open(10,file='multflt.dat')
      open(11,file=trim(fname))
         faultname=' '
         do i=1,nrflts
            read(10,*)num,faultname(i),faultmean(i),faultstd(i)
         enddo
         write(11,'(a)')'MULTFLT'
         do i=1,nrflts
            write(11,'(1x,a,a,a,2x,f8.5,2x,a)')sep,trim(faultname(i)),sep,mem4%multflt(i),' /'
         enddo
         write(11,'(a)')'/'
      close(11)
      close(10)
#endif

      print *,' ==> Random prior is genereated using iniens program'
      print *,' ==> Ready to run reference case.'
      print *
   endif
   return

end subroutine
end module
