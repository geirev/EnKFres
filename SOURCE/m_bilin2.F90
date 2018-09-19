module m_bilin2
contains
subroutine bilin2(old,onx,ony,oxref,oyref,odx,ody,new,nx,ny,newxpos,newypos,lm)   
   implicit none
   integer, intent(in) :: onx         ! x-dimension of old field
   integer, intent(in) :: ony         ! y-dimension of old field
   real, intent(in)    :: old(onx,ony)! old grid
   real, intent(in)    :: oxref       ! reference point
   real, intent(in)    :: oyref       ! reference point
   real, intent(in)    :: odx         ! grid spacing in old grid
   real, intent(in)    :: ody         ! grid spacing in old grid

   integer, intent(in) :: nx          ! x-dimension of old field
   integer, intent(in) :: ny          ! y-dimension of old field
   real, intent(out)   :: new(nx,ny)  ! New interpoyed field
   real, intent(in)    :: newxpos(nx,ny) ! Longitudes for new grid
   real, intent(in)    :: newypos(nx,ny) ! Latitudes for new grid
   logical, intent(in) :: lm(nx,ny)      ! mask




   integer i,j
   integer ipos    !  index of i-pivot grid point in old grid
   integer jpos    !  index of j-pivot grid point in old grid

   real aa,bb,a1,a2,a3,a4


   new=0.0
   do j=1,ny
   do i=1,nx
      if (lm(i,j)) then
      ipos=int((newxpos(i,j)-oxref)/odx+1.0)
      jpos=int((newypos(i,j)-oyref)/ody+1.0)
      if (ipos >= onx) print *,'bilin2: ipos>onx :',i,j,ipos,onx
      if (jpos >= ony) print *,'bilin2: jpos>ony :',i,j,jpos,ony
      if (ipos < 1) print *,'bilin2: ipos<1 :',i,j,ipos
      if (jpos < 1) print *,'bilin2: jpos<1 :',i,j,jpos


      aa=(newxpos(i,j) - oxref-real(ipos-1)*odx)/odx
      bb=(newypos(i,j) - oyref-real(jpos-1)*ody)/ody

      a1=(1.0-aa)*(1.0-bb)
      a2=aa*(1.0-bb)
      a3=aa*bb
      a4=(1.0-aa)*bb

      new(i,j) = a1*old(ipos  ,jpos  )&
                +a2*old(ipos+1,jpos  )&
                +a3*old(ipos+1,jpos+1)&
                +a4*old(ipos  ,jpos+1)
      endif
   enddo
   enddo

end subroutine bilin2
end module m_bilin2
