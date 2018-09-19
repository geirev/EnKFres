module m_ens2ecl
contains
subroutine ens2ecl(iopt,iens,ecldir,ensdir)
   use mod_dimensions
   use mod_states
   use mod_eclrestart
   use mod_fnames
   use m_get_nrens
   use m_eclipse_write
   use m_ensio
   use m_getmask
#ifdef GAUSS2
   use mod_facies
#endif
   implicit none
#ifdef MULTFLT
   character(len=10) faultname(nrflts)
   real faultmean(nrflts),faultstd(nrflts)
#endif
#ifdef EQUIL
   real eqdepth(eqldims), eqp(eqldims),      eqwoc(eqldims),   eqpc1(eqldims)
   real eqgoc(eqldims),   eqpc2(eqldims),    eqrs(eqldims),    eqrv(eqldims)
   real eqacc(eqldims),   eqwocstd(eqldims), eqgocstd(eqldims)
#endif

   integer, intent(in) :: iopt
   integer, intent(in) :: iens

   character(len=5) ident
   character(len=9) rident
   character(len=*) ecldir
   character(len=*) ensdir

   type(states4) mem4
   logical ex
   integer ird,nn,num
   integer reclA,reclB,i,iflg,irec,iwrt,k
   integer ishape1(1)
   integer ishape3(3)
   character(len=1) :: sep=''''
   real, parameter :: pi=3.1415927
   integer, parameter :: en=1

! Eclipse static variables
   integer, parameter :: ndim=nx*ny*nz
   real*4 permx(ndim)
   real*4 permz(ndim)
   real*4 poro(ndim)
   real*4 dummy(nx,ny,nz)

   open(13,file='interface.log')

   if (iopt /= 23) then
! Reading header from static file and allocating all Eclipse variables
      inquire(file=trim(ensSfile),exist=ex)
      if (.not.ex) then
         print *,'ERROR (ens2ecl) Static file does not exist: ',trim(ensSfile)
         return
      endif



! Used without reclS to get reclB from file
      inquire(iolength=reclA)reclB,rident,nrfields,fieldname,fieldsize,fieldtype
      open(10,file=trim(ensSfile),form='unformatted',access='direct',recl=reclA)
         read(10,rec=1)reclB,rident,nn,fieldname,fieldsize,fieldtype
      close(10)


!     open(10,file=trim(ensSfile),form='unformatted',access='direct',recl=reclS)
      open(10,file=trim(ensSfile),form='unformatted',access='direct',recl=reclB)
         if (iopt==3) then
            read(10,rec=1)reclB,rident,nn,fieldname,fieldsize,fieldtype
         else
            read(10,rec=iens)reclB,rident,nn,fieldname,fieldsize,fieldtype
         endif
         do i=1,nrfields
            if (fieldsize(i) == -1) exit
            write(13,'(a8,i10,tr2,a4)')fieldname(i),fieldsize(i),fieldtype(i)
         enddo
         nnfields=i-1
      close(10)

      call resallocate()

      do i=1,nnfields
         if (fieldsize(i) == -1) exit
         select case (trim(fieldname(i)))
#include "res_writeeclipse1.inc"
            case default
               write(*,*)'ERROR (ens2ecl) nonexistent variable: ',trim(fieldname(i))
               close(10)
               write(13,'(a)')'interface (m_ens2ecl) problem'
               write(13,'(a)')'nonexistent variable: ',fieldname(i)
               write(13,'(a)')'Inconsistency between ens2ecl and ecl2ens'
               write(13,'(a)')'Probably you forgot to include ',trim(fieldname(i)),' in ens2ecl'
               return
         end select
      enddo
   endif

   if (iopt==3) then
      irec=1
      ird=read_static(irec)
      do irec=2,iens
         iwrt=write_static(irec)
      enddo
      close(10)
      call resdeallocate
      return
   endif

   if (iopt /= 23 ) then
      ird=read_static(iens)
   endif

   if (iopt == 2 .or. iopt==22 .or. iopt==23) then
! Reading ensemble file
      write(13,'(2a)')'interface: reads model file = ',trim(ensXfile)
!      nrens=get_nrens(trim(ensXfile))
      write(13,'(a,i4,a,i4)')'Ensemble size = ',nrens,'--- iens= ',iens
      if (iens < 1 .or. nrens < iens) then
         write(*,'(a,i4,a,i4)')'Ensemble size = ',nrens,'--- iens= ',iens
         write(*,*)'ERROR (ens2ecl) invalid iens value'
         return
      endif


      iflg=open_ensemblefile(trim(ensXfile),'old')
      iflg=read_ensmem(iens,mem4)
      close(10)

!      print *,'ensfile: ',trim(ensXfile)
!      do i=1,eqldims
!      print '(a,2f8.2)','WOC GOC : ',mem4%equilWOC(i),mem4%equilGOC(i)
!      enddo





      ishape1(1)=ndim

      PRESSURE(1:fieldsize(ipres)) =mem4%pres(1:fieldsize(ipres))
      SGAS(1:fieldsize(isgas))     =mem4%sg(1:fieldsize(isgas))
      SWAT(1:fieldsize(iswat))     =mem4%sw(1:fieldsize(iswat))
      RS(1:fieldsize(irs))         =mem4%rs(1:fieldsize(irs))

#ifdef VAPOIL
      RV(1:fieldsize(irv))         =mem4%rv(1:fieldsize(irv))
#endif

#ifdef GAUSS2
      open(10,file=trim(ecldir)//'gauss.uf',form='unformatted')
         write(10)mem4%gauss1,mem4%gauss2
      close(10)

      gauss1=mem4%gauss1
      gauss2=mem4%gauss2

      call facies_def
      call facies_compute

      ishape1=(/nx*ny*nz/)
      poro=reshape(poro3d,ishape1)
      print *,poro(10*10),poro(20*20),poro(30*30)

      permx=10**(9.02*poro+0.77)
      permz=0.31*permx + 3.12

#else
      dummy=0.0
      PERMX   =reshape(unpack(mem4%permx,lmask,dummy),ishape1)
      PERMZ   =reshape(unpack(mem4%permz,lmask,dummy),ishape1)
      PORO    =reshape(unpack(mem4%poro,lmask,dummy),ishape1)
#endif

#ifdef MULTZ
      if (iopt /= 22) then
         open(10,file=trim(ecldir)//'MULTZ.INC')
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

         open(10,file=trim(ecldir)//'multz.uf',form='unformatted')
            write(10)mem4%multz
         close(10)
      endif
#endif

#ifdef MULTFLT
      if (iopt /= 22) then
         open(10,file='multflt.dat')
            do i=1,nrflts
               read(10,*)num,faultname(i),faultmean(i),faultstd(i)
            enddo
         close(10)

         open(10,file=trim(ecldir)//'MULTFLT.INC')
            write(10,'(a)')'MULTFLT'
            do i=1,nrflts
               write(10,'(1x,a,a,a,t12,f8.5,2x,a)')sep,trim(faultname(i)),sep,mem4%multflt(i),' /'
            enddo
            write(10,'(a)')'/'
         close(10)

         open(10,file=trim(ecldir)//'multflt.uf',form='unformatted')
            write(10)mem4%multflt
         close(10)
      endif
#endif

#ifdef EQUIL
      if (iopt /= 22) then
         open(10,file='equil.dat')
            do i=1,eqldims
               read(10,*)eqdepth(i), eqp(i), eqwoc(i), eqpc1(i), eqgoc(i),&
                                 eqpc2(i), eqrs(i), eqrv(i), eqacc(i), eqwocstd(i), eqgocstd(i)
            enddo
         close(10)

         open(10,file=trim(ecldir)//'EQUIL.INC')
            write(10,'(a)')'EQUIL'
            write(10,'(a)')'-- depth       P     WOC      Pc     GOC      Pc  Rs(d) Rv(d) Acc(IIP)'
            do i=1,eqldims
               write(10,'(6f8.2,3i5,a)')   &
                                 eqdepth(i), eqp(i), mem4%equilWOC(i), eqpc1(i), mem4%equilGOC(i),&
                                 eqpc2(i), nint(eqrs(i)), nint(eqrv(i)), nint(eqacc(i)),' /'
            enddo
         close(10)

         open(10,file=trim(ecldir)//'equil.uf',form='unformatted')
            write(10)mem4%equilWOC,mem4%equilGOC
         close(10)
      endif
#endif

#ifdef WELLSTATE
!      XGRP(1:fieldsize(ixgrp))=mem4%XGRP(1:fieldsize(ixgrp))
!      RSEG(1:fieldsize(irseg))=mem4%RSEG(1:fieldsize(irseg))
!      XWEL(1:fieldsize(ixwel))=mem4%XWEL(1:fieldsize(ixwel))
!      SCON(1:fieldsize(iscon))=mem4%SCON(1:fieldsize(iscon))
!      XCON(1:fieldsize(ixcon))=mem4%XCON(1:fieldsize(ixcon))
#endif


! Write Eclipse restart files

      if (iopt == 2 .or. iopt == 23) then
         write(13,*)'interface: writes file      = ',trim(ecldir)//'PERMX.INC'
         open(10,file=trim(ecldir)//'PERMX.INC')
            write(10,'(a)')'PERMX'
            write(10,'(5(e14.6))')permx(1:ndim)
            write(10,'(a)')' /'
         close(10)

         write(13,*)'interface: writes file      = ',trim(ecldir)//'PERMZ.INC'
         open(10,file=trim(ecldir)//'PERMZ.INC')
            write(10,'(a)')'PERMZ'
            write(10,'(5(e14.6))')permz(1:ndim)
            write(10,'(a)')' /'
         close(10)

         write(13,*)'interface: writes file      = ',trim(ecldir)//'PORO.INC'
         open(10,file=trim(ecldir)//'PORO.INC')
            write(10,'(a)')'PORO'
            write(10,'(5(e14.6))')poro(1:ndim)
            write(10,'(a)')' /'
         close(10)

         write(13,*)'interface: writes file      = ',trim(ecldir)//'poroperm.uf'
         open(10,file=trim(ecldir)//'poroperm.uf',form='unformatted')
            write(10)mem4%poro,mem4%permx,mem4%permz
         close(10)

      endif

      if (iopt == 23) then
         write(13,*)'return from interface:ens2ecl after writing poro/perm and gauss'
         return
      endif

      write(13,*)'interface: writes file      = ',trim(eclFfile)
      open(10,file=trim(eclFfile))
      do i=1,nnfields
         write(13,'(a8,i10,tr2,a4)')fieldname(i),fieldsize(i),fieldtype(i)
         select case (trim(fieldname(i)))
#include "res_writeeclipse2.inc"
         end select
      enddo
      close(10)
   endif
   call resdeallocate
   close(13)

end subroutine ens2ecl
end module m_ens2ecl
