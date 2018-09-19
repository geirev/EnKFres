module m_aveorig
contains
subroutine aveorig(f1,mask,nx,ny,ave1,var1,minf,maxf,pdf,xx,nn)
   integer, intent(in)  :: nn
   integer, intent(in)  :: nx
   integer, intent(in)  :: ny
   real,    intent(in)  :: f1(nx,ny)
   integer, intent(in)  :: mask(nx,ny)
   real,    intent(out) :: pdf(nn)
   real,    intent(out) :: xx(nn)
   real,    intent(out) :: minf
   real,    intent(out) :: maxf
   real,    intent(out) :: ave1
   real,    intent(out) :: var1

   integer num,i,j,n
   real deltax,minf1,maxf1
   real, parameter :: extreme=1.0E10

   ave1=0.0; var1=0.0

   num=max(sum(mask(:,:)),1)
   do j=1,ny
   do i=1,nx
      ave1=ave1+real(mask(i,j))*f1(i,j)
   enddo
   enddo
   ave1=ave1/real(num)

   do j=1,ny
   do i=1,nx
      var1=var1+real(mask(i,j))*(f1(i,j)-ave1)**2
   enddo
   enddo
   var1=var1/real(num)

   minf1=extreme
   maxf1=-extreme
   do j=1,ny
   do i=1,nx
      if (mask(i,j) == 1) then
         minf1=min(f1(i,j),minf1)
         maxf1=max(f1(i,j),maxf1)
      endif
   enddo
   enddo
   minf=minf1
   maxf=maxf1
   if (minf==extreme) minf=0.0
   if (maxf==-extreme) maxf=0.0

   if (minf1 == maxf1) then
      minf1= minf1-0.1
      maxf1= maxf1+0.1
   endif

   deltax=(maxf1-minf1)/float(nn-1)

   do i=1,nn
      xx(i)=minf1+real(i-1)*deltax
   enddo

   pdf=0.0
   do j=1,ny
   do i=1,nx
      if (mask(i,j) == 1) then
         n=int((f1(i,j)-minf1)/deltax+1.0000001)
         pdf(n) = pdf(n)+1.0
      endif
   enddo
   enddo

end subroutine aveorig
end module m_aveorig
