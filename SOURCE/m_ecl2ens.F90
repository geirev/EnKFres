module m_ecl2ens
contains
subroutine ecl2ens(iens,ecldir)
   use mod_dimensions
   use mod_states
! INMEM
!  use mod_ensemble
   use mod_eclrestart
   use mod_eclsummary
   use mod_eclfsmspec
   use mod_fnames

   use m_getmask
   use m_get_nrens
   use m_eclipse_read
   use m_ensio

#ifdef GAUSS2
   use mod_facies
#endif

   implicit none
   integer, intent(in) :: iens
   character(len=*), intent(in) :: ecldir

   character(len=80) filename
   character(len=5) ident
   character(len=9) rident

   type(states4) mem4
   logical ex
   integer ird
   integer i,iflg
   integer ishape3(3)
   integer ishape1(1)

   real, parameter :: pi=3.1415927

   integer nrwells,iw,iwrt,k
   character(len=8), allocatable :: wells(:)

! Eclipse static variables
   integer, parameter :: ndim=nx*ny*nz
   real*4 permx(ndim)
   real*4 permz(ndim)
   real*4 poro(ndim)

   open(13,file='interface.log')

   mem4=0.0

#ifdef GAUSS2
   write(13,*)'interface(m_ecl2ens): reads file      = ','gauss.uf'
   filename(:)=' '
   i=len_trim(ecldir)
   filename(1:i)=trim(ecldir)
   filename(i+1:i+8)='gauss.uf'
   inquire(file=trim(filename),exist=ex)
   if (.not.ex) then
      print '(3a)','ERROR (interface,m_ecl2ens): ',trim(filename),' does not exist.'
      return
   endif
   open(10,file=trim(filename),form='unformatted')
      read(10)mem4%gauss1,mem4%gauss2
   close(10)

   gauss1=mem4%gauss1
   gauss2=mem4%gauss2

   call facies_def
   call facies_compute

   ishape1=(/nx*ny*nz/)
   poro=reshape(poro3d,ishape1)
   permx=10**(9.02*poro+0.77)
   permz=0.31*permx + 3.12

#else
   filename(:)=' '
   filename=trim(ecldir)//'poroperm.uf'
   inquire(file=trim(filename),exist=ex)
   if (ex) then
      write(13,*)'interface: reads file      = ',trim(filename)
      open(10,file=trim(ecldir)//'poroperm.uf',form='unformatted')
         read(10)mem4%poro,mem4%permx,mem4%permz
      close(10)
   else
      filename(:)=' '
      i=len_trim(ecldir)
      filename(1:i)=trim(ecldir)
      filename(i+1:i+9)='PERMX.INC'
      write(13,*)'interface(m_ecl2ens): reads file      = ',trim(filename)
      inquire(file=trim(filename),exist=ex)
      if (.not.ex) then
         print '(3a)','ERROR (interface,m_ecl2ens): ',trim(filename),' does not exist.'
         return
      endif
      open(10,file=trim(filename),access='sequential')
         read(10,'(a5)')ident
         ird=read_perm(ndim,permx)
      close(10)

      filename(:)=' '
      i=len_trim(ecldir)
      filename(1:i)=trim(ecldir)
      filename(i+1:i+9)='PERMZ.INC'
      write(13,*)'interface(m_ecl2ens): reads file      = ',trim(filename)
      inquire(file=trim(filename),exist=ex)
      if (.not.ex) then
         print '(3a)','ERROR (interface,m_ecl2ens): ',trim(filename),' does not exist.'
         return
      endif
      open(10,file=trim(filename),access='sequential')
         read(10,'(a5)')ident
         ird=read_perm(ndim,permz)
      close(10)
      
      filename(:)=' '
      i=len_trim(ecldir)
      filename(1:i)=trim(ecldir)
      filename(i+1:i+8)='PORO.INC'
      write(13,*)'interface(m_ecl2ens): reads file      = ',trim(filename)
      inquire(file=trim(filename),exist=ex)
      if (.not.ex) then
         print '(3a)','ERROR (interface,m_ecl2ens): ',trim(filename),' does not exist.'
         return
      endif
      open(10,file=trim(filename),access='sequential')
         read(10,'(a5)')ident
         ird=read_perm(ndim,poro)
      close(10)

      ishape3=(/nx,ny,nz/)
      mem4%permx=pack(reshape(permx,ishape3),lmask)
      mem4%permz=pack(reshape(permz,ishape3),lmask)
      mem4%poro =pack(reshape(poro,ishape3), lmask)

   endif
#endif


#ifdef MULTZ
   filename(:)=' '
   i=len_trim(ecldir)
   filename(1:i)=trim(ecldir)
   filename(i+1:i+8)='multz.uf'
   write(13,*)'interface(m_ecl2ens): reads file      = ',trim(filename)
   inquire(file=trim(filename),exist=ex)
   if (.not.ex) then
!     print *,'multz.uf file does not exist, values set to 1.0'
      mem4%multz=1.0
   else
      open(10,file=trim(filename),form='unformatted')
         read(10)mem4%multz
      close(10)
   endif
#endif
      
#ifdef MULTFLT
   filename(:)=' '
   i=len_trim(ecldir)
   filename(1:i)=trim(ecldir)
   filename(i+1:i+10)='multflt.uf'
   write(13,*)'interface(m_ecl2ens): reads file      = ',trim(filename)
   inquire(file=trim(filename),exist=ex)
   if (.not.ex) then
!     print *,'multflt.uf file does not exist, values set to 0.0'
      mem4%multflt=0.0
   else
      open(10,file=trim(filename),form='unformatted')
         read(10)mem4%multflt
      close(10)
   endif
#endif
      
#ifdef EQUIL
   filename(:)=' '
   i=len_trim(ecldir)
   filename(1:i)=trim(ecldir)
   filename(i+1:i+8)='equil.uf'
   write(13,*)'interface(m_ecl2ens): reads file      = ',trim(filename)
   inquire(file=trim(filename),exist=ex)
   if (.not.ex) then
!     print *,'equil.uf file does not exist, values set to 0.0'
      mem4%equilWOC=0.0
      mem4%equilGOC=0.0
   else
      open(10,file=trim(filename),form='unformatted')
         read(10)mem4%equilWOC,mem4%equilGOC
      close(10)
   endif
#endif
      

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Eclipse summary file contains well information
   call fsmallocate()


! Eclipse summary header file
   write(13,*)'interface(m_ecl2ens): reads file      = ',trim(eclHfile)

   inquire(file=trim(eclHfile),exist=lhead)
   if (.not.lhead) write(13,*)'interface: Eclipse header file does not exist'

   if (lhead) then
      open(10,file=trim(eclHfile),form='formatted',access='sequential')
      ihead=0
      do 
         ihead=ihead+1
         read(10,'(t3,a8,t13,i11,t26,a4)',end=998)headname(ihead),headsize(ihead),headtype(ihead)
         write(13,*)'HEADER: ',headname(ihead),headsize(ihead),headtype(ihead)

         select case (trim(headname(ihead)))
#include "fsm_readeclipse.inc"
         case default
            print *,'ERROR: variable not contained in mod_eclsummary: ',headname(ihead)
            close(10)
            write(*,'(a)')'interface (m_ecl2ens) problem'
            write(*,'(a,a)')'Summary file variable not contained in mod_eclsummary: ',trim(headname(ihead))
            write(*,'(a)')'You need to follow the standard procedure for including'
            write(*,'(a)')'new Eclipse vaiables.  See instructions in interface.F90.'
            stop
         end select
      enddo

 998  close(10)
   endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Eclipse summary file contains well information
   call sumallocate()

   write(13,*)'interface: reads file      = ',trim(eclAfile)
   inquire(file=trim(eclAfile),exist=lsumm)
   if (.not.lsumm) write(13,*)'Eclipse summary file does not exist (assume not assimilation step)'

   if (lsumm) then
      open(10,file=trim(eclAfile),form='formatted',access='sequential')

      isumm=1 
      read(10,'(t3,a8,t13,i11,t26,a4)')summname(isumm),summsize(isumm),summtype(isumm)
      write(13,*)'HEADER: ',summname(isumm),summsize(isumm),summtype(isumm)

      if (summname(isumm) /= 'SEQHDR') then
         close(10)
         write(*,'(a)')'interface (m_ecl2ens): SEQHDR problem'
         stop 'interface: SEQHDR'
      endif

      deallocate( SEQHDR )
      allocate(SEQHDR(summsize(isumm)))
      ird=read_integer(summsize(isumm),SEQHDR,summtype(isumm))

      do 

         isumm=2 
         read(10,'(t3,a8,t13,i11,t26,a4)',end=90)summname(isumm),summsize(isumm),summtype(isumm)
         write(13,*)'HEADER: ',summname(isumm),summsize(isumm),summtype(isumm)
         if (summname(isumm) /= 'MINISTEP') stop 'interface: MINISTEP'
         deallocate(MINISTEP); allocate(MINISTEP(summsize(isumm)))
         ird=read_integer(summsize(isumm),MINISTEP,summtype(isumm))

         isumm=3 
         read(10,'(t3,a8,t13,i11,t26,a4)')summname(isumm),summsize(isumm),summtype(isumm)
         write(13,*)'HEADER: ',summname(isumm),summsize(isumm),summtype(isumm)
         if (summname(isumm) /= 'PARAMS') stop 'interface: PARAMS'
         deallocate(PARAMS); allocate(PARAMS(summsize(isumm)))
         ird=read_real(summsize(isumm),PARAMS,summtype(isumm))

         cycle
         90 exit

      enddo

      close(10)

   endif


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


   write(13,*)'interface: reads file      = ',trim(eclFfile)
   inquire(file=trim(eclFfile),exist=ex)
   if (.not.ex) then
      write(13,*)'eclipse restart file does not exist.'
      call fsmdeallocate()
      call sumdeallocate()
      return
   endif

   call resallocate()
   open(10,file=trim(eclFfile),form='formatted',access='sequential')
   fieldsize=-1
   i=0
   do 
      i=i+1
      read(10,'(t3,a8,t13,i11,t26,a4)',end=999)fieldname(i),fieldsize(i),fieldtype(i)
      write(13,*)'HEADER: ',fieldname(i),fieldsize(i),fieldtype(i)

      select case (trim(fieldname(i)))
#include "res_readeclipse.inc"
         case default
            write(13,*)'variable not contained in mod_eclsummary: ',fieldname(i)
            close(10)
            write(*,'(a)')'interface (m_ecl2ens) problem'
            write(*,'(a,a)')'Restart file variable not contained in mod_eclsummary: ',trim(headname(ihead))
            write(*,'(a)')'You need to follow the standard procedure for including'
            write(*,'(a)')'new Eclipse vaiables.  See instructions in interface.F90.'
            stop
         end select
      enddo

      if (INTEHEAD(9) /= nx .or. INTEHEAD(10) /= ny .and. INTEHEAD(11) /= nz) then
         print *,'interface problem:  Grid dimensions not set consistently'
         print *,'   INTEHEAD(9:11)= ', INTEHEAD(9:11)
         print *,'   nx,ny,nz    =   ', nx,ny,nz
      endif
      if (INTEHEAD(12) /= ndim) then
         print *,'interface warning: grid cells have been cancelled by Eclipse'
         print *,'   INTEHEAD(12)= ', INTEHEAD(12)
         print *,'   nx*ny*nz    = ', ndim
      endif

 999 nnfields=i-1
   close(10)

! File with number of actic cells for each member for checking.
   open(10,file='activecells.tmp',position='append')
      write(10,'(i5,i10)')iens,INTEHEAD(12)
   close(10)




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Writing static file

   iwrt=write_static(iens)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Copy state variable to states type
   mem4%pres(1:fieldsize(ipres))=PRESSURE(1:fieldsize(ipres))
   mem4%sw(1:fieldsize(iswat))=SWAT(1:fieldsize(iswat))
   mem4%sg(1:fieldsize(isgas))=SGAS(1:fieldsize(isgas))
   mem4%rs(1:fieldsize(irs))=RS(1:fieldsize(irs))
#ifdef VAPOIL
   if (irv > 0) then
      mem4%rv(1:fieldsize(irv))=RV(1:fieldsize(irv))
   else
      mem4%rv=0.0
   endif
#endif

#ifdef WELLSTATE
   mem4%XGRP(1:fieldsize(ixgrp))=XGRP(1:fieldsize(ixgrp))
   mem4%RSEG(1:fieldsize(irseg))=RSEG(1:fieldsize(irseg))
   mem4%XWEL(1:fieldsize(ixwel))=XWEL(1:fieldsize(ixwel))
   mem4%SCON(1:fieldsize(iscon))=SCON(1:fieldsize(iscon))
   mem4%XCON(1:fieldsize(ixcon))=XCON(1:fieldsize(ixcon))
#endif


! dates information
   open(10,file='days.dat',position='append',access='sequential')
      write(10,'(i4.4,F10.2,i5,i3,i3)')INTEHEAD(69),DOUBHEAD(1),INTEHEAD(67),INTEHEAD(66),INTEHEAD(65)
   close(10)
   call system('mv days.dat days.tmp; cat days.tmp | sort -u > days.dat; rm days.tmp')



! Copy well information to states type
   if (lsumm .and. lhead) then
      write(13,*)'interface: Identify wells'
      open(10,file='tmpw')
         do i=3,DIMENS(1)
            write(10,'(a)')trim(WGNAMES(i)(2:9))
         enddo
      close(10)
      call system('cat tmpw | sort -u > wells.tmp')
      
      inquire(file='wells.dat',exist=ex)
      if (.not.ex) then
         call system('mv wells.tmp wells.dat')
         print *,'interface (ecl2ens): Check that wells.dat file is ok for rest of simulation'
      endif

      open(10,file='wells.dat')
         allocate(wells(1))
         i=1
         do
            read(10,'(a)',end=96)wells(1)
            i=i+1
            cycle
            96 exit
         enddo
         nrwells=i-1
         deallocate(wells)

         write(13,*)'nrwells=',nrwells
         allocate(wells(nrwells))
         rewind(10)
         do i=1,nrwells
            read(10,'(a)')wells(i)
            write(13,'(a,i4,tr2,a)')'Well ',i,wells(i)
         enddo
      close(10)

      write(13,*)'Copies well information to states variable'
      do iw=1,nrwells
         write(13,*)'iw=',iw,DIMENS(1),trim(wells(iw))
         do i=3,DIMENS(1)
            if ( trim(WGNAMES(i)(2:9)) == trim(wells(iw)) ) then
               select case (trim(KEYWORDS(i)(2:9)))
               case ('WTHP'); mem4%THP(iw)=PARAMS(i)
               case ('WBHP'); mem4%BHP(iw)=PARAMS(i)
               case ('WOPR'); mem4%OPR(iw)=PARAMS(i)
               case ('WGPR'); mem4%GPR(iw)=PARAMS(i)
               case ('WWPR'); mem4%WPR(iw)=PARAMS(i)
               case ('WWCT'); mem4%WCT(iw)=PARAMS(i)
               case ('WGOR'); mem4%GOR(iw)=PARAMS(i)
               case ('WOPT'); mem4%OPT(iw)=PARAMS(i)
               case ('WGPT'); mem4%GPT(iw)=PARAMS(i)
               case ('WWPT'); mem4%WPT(iw)=PARAMS(i)
               end select
            endif
         enddo
         write(13,'(a,i3,7e12.2)')'well vars: ',iw,mem4%THP(iw),mem4%BHP(iw),&
                        mem4%OPR(iw),mem4%GPR(iw),mem4%WPR(iw),mem4%WCT(iw),mem4%GOR(iw)
      enddo
      deallocate(wells)
   else
      write(13,*)'interface: No well information stored in ensemble file: ',trim(ensXfile)
   endif

   write(13,*)'interface: writes file     = ',trim(ensXfile)
   iflg=open_ensemblefile(trim(ensXfile),'unknown')
   rident='FORECAST '
   iflg=write_ensmem(iens,rident,mem4)
   close(10)

! INMEM
!   A(iens)=mem4

   call fsmdeallocate()
   call sumdeallocate()
   call resdeallocate()



end subroutine ecl2ens
end module m_ecl2ens
