module m_readorig
   use mod_dimensions
   real, public, dimension(nx,ny,nz) :: permxo, permzo, poroo
   real, public, dimension(nz) :: Ax,  Az, Ap
   real, public, dimension(nz) :: Vx,  Vz, Vp
   real, public, dimension(nz) :: minx,  minz, minp
   real, public, dimension(nz) :: maxx,  maxz, maxp
   logical, public :: expermx,expermz,exporo
contains 
subroutine readorig(mask)
   use mod_localdefs
   use m_aveorig
   use m_eclipse_read
   implicit none
   integer, intent(in),  dimension(nx,ny,nz) :: mask

   integer, parameter :: ndim=nx*ny*nz
   integer, parameter :: nn=50
   real, dimension(nn,nz) :: pdfx,pdfz,pdfp
   real, dimension(nn,nz) :: xx,zz,pp
   real*4, dimension(nx,ny,nz) :: permxorig, permzorig, poroorig
   integer ird, k, i
   character(len=5) ident
   character(len=80) fname

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading original poro, permx, permz from BC model
      fname(:)=' '
      i=len_trim(pridir)
      fname(1:i)=trim(pridir)

      fname(i+1:i+8)='PORO.INC'
      inquire(file=trim(fname),exist=exporo)
      if (exporo) then
         print *,'readorig: reads file      = ',trim(fname)
         open(10,file=trim(fname),access='sequential')
            read(10,'(a5)')ident; print *,'Ident=',ident
            ird=read_perm(ndim,poroorig)
         close(10)
         poroo=poroorig
      else
         print *,trim(fname),' does not exist'
      endif

      fname(i+1:i+9)='PERMX.INC'
      inquire(file=trim(fname),exist=expermx)
      if (expermx) then
         print *,'readorig: reads file      = ',trim(fname)
         open(10,file=trim(fname),access='sequential')
            read(10,'(a5)')ident; print *,'Ident=',ident
            ird=read_perm(ndim,permxorig)
         close(10)
         permxo=permxorig
      else
         print *,trim(fname),' does not exist'
      endif

      fname(i+1:i+9)='PERMZ.INC'
      inquire(file=trim(fname),exist=expermz)
      if (expermx) then
         print *,'readorig: reads file      = ',trim(fname)
         open(10,file=trim(fname),access='sequential')
            read(10,'(a5)')ident; print *,'Ident=',ident
            ird=read_perm(ndim,permzorig)
         close(10)
         permzo=permzorig
      else
         print *,trim(fname),' does not exist'
      endif


! Computing statistical parameters from original fields
   if (expermx .and. expermz .and. exporo) then

      do k=1,nz
         call aveorig(permxo(:,:,k),mask(:,:,k),nx,ny,Ax(k),Vx(k),minx(k),maxx(k),pdfx(:,k),xx(:,k),nn)
         call aveorig(permzo(:,:,k),mask(:,:,k),nx,ny,Az(k),Vz(k),minz(k),maxz(k),pdfz(:,k),zz(:,k),nn)
         call aveorig(poroo(:,:,k), mask(:,:,k),nx,ny,Ap(k),Vp(k),minp(k),maxp(k),pdfp(:,k),pp(:,k),nn)
      enddo

      open(10,file='pdf.dat')
      write(10,*)'TITLE = "PDFs"'
      write(10,*)'VARIABLES = "permx" "permz" "poro" "pdf permx" "pdf permz" "pdf poro"'

      do k=1,nz
      write(10,'(a,i3,a,i3)')' ZONE T="pdfs for layer',k,'"  F=POINT, I= ',nn
      do i=1,nn
         write(10,'(6g13.4)')xx(i,k),zz(i,k),pp(i,k),pdfx(i,k),pdfz(i,k),pdfp(i,k)
      enddo
      enddo
      close(10)

      open(10,file='AV.dat')
         do k=1,nz
            write(10,'(a,i3,6(a,f12.4))')'i=',k,'; avex(i)=',Ax(k),'; stdx(i)=',sqrt(Vx(k)),'; xl(i)=',minx(k),'; xu(i)=',maxx(k)
         enddo
         write(10,*)' '
         do k=1,nz
            write(10,'(a,i3,6(a,f12.4))')'i=',k,'; avez(i)=',Az(k),'; stdz(i)=',sqrt(Vz(k)),'; zl(i)=',minz(k),'; zu(i)=',maxz(k)
         enddo
         write(10,*)' '
         do k=1,nz
            write(10,'(a,i3,6(a,f12.4))')'i=',k,'; avep(i)=',Ap(k),'; stdp(i)=',sqrt(Vp(k)),'; pl(i)=',minp(k),'; pu(i)=',maxp(k)
         enddo
      close(10)

      print *,'Statistical data dumped to pdf.dat and AV.dat'
   endif

end subroutine
end module
