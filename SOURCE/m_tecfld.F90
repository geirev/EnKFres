module m_tecfld
contains
subroutine tecfld(fname,ii,jj,nr,fld)
   implicit none
   character(len=*), intent(in) :: fname
   integer,          intent(in) :: ii,jj,nr
   real,             intent(in) :: fld(ii,jj,nr)
   integer m,i,j
   logical lopen
   integer iunit
   character(len=80) totfname
   integer ilen

   do iunit=10,20
      inquire(unit=iunit,opened=lopen)
      if (.not.lopen) then
!         totfname(:)=' '
!         totfname(1:4)='tec_'
!         ilen=len_trim(fname)
!         totfname(5:5+ilen-1)=fname(1:ilen)
!         totfname(5+ilen:5+ilen+3)='.dat'
!        open(iunit,file='tec_'//fname//'.dat',status='unknown')

         totfname(:)=' '
         totfname='tec_'//trim(fname)//'.dat'
         open(iunit,file=trim(totfname),status='unknown')
            write(iunit,*)'TITLE = "',fname,'"'
            write(iunit,*)'VARIABLES = "i-index" "j-index" '
            write(iunit,'(5(a,i3,a))')(' "F',i,'"',i=1,nr)
            write(iunit,'(a,i3,a,i3,a)')' ZONE  F=BLOCK, I=',ii,', J=',jj,', K=1'
            write(iunit,'(30I4)')((i,i=1,ii),j=1,jj)
            write(iunit,'(30I4)')((j,i=1,ii),j=1,jj)
            do m=1,nr
               write(iunit,900)((fld(i,j,m),i=1,ii),j=1,jj)
            enddo
         close(iunit)
         exit
      endif
   enddo
 900 format(10(1x,e12.5))
end subroutine tecfld

subroutine tecfld2(fname,ii,jj,nr,fld,imask,x1,y1,dx,dy)
   implicit none
   character(len=*), intent(in) :: fname
   integer,          intent(in) :: ii,jj,nr
   real,             intent(in) :: fld(ii,jj,nr)
   integer, optional, intent(in) :: imask(ii,jj)
   real,             intent(in) :: dx,dy,x1,y1
   integer mask(ii,jj)
   integer m,i,j
   logical lopen
   integer iunit
   character(len=80) totfname
   integer ilen

   if (present(imask)) then
      mask=imask
   else
      mask=1
   endif

   do iunit=10,20
      inquire(unit=iunit,opened=lopen)
      if (.not.lopen) then
!         totfname(:)=' '
!         totfname(1:4)='tec_'
!         ilen=len_trim(fname)
!         totfname(5:5+ilen-1)=fname(1:ilen)
!         totfname(5+ilen:5+ilen+3)='.dat'
!         open(iunit,file='tec_'//fname//'.dat',status='unknown')
         totfname(:)=' '
         totfname='tec_'//trim(fname)//'.dat'
         open(iunit,file=trim(totfname),status='unknown')
            write(iunit,*)'TITLE = "',fname,'"'
            write(iunit,*)'VARIABLES = "i-index" "j-index" "x" "y" "mask" '
            write(iunit,'(5(a,i3,a))')(' "F',i,'"',i=1,nr)
            write(iunit,'(a,i3,a,i3,a)')' ZONE  F=BLOCK, I=',ii,', J=',jj,', K=1'
            write(iunit,'(30I4)')((i,i=1,ii),j=1,jj)
            write(iunit,'(30I4)')((j,i=1,ii),j=1,jj)
            write(iunit,900)((x1+dx*real(i-1),i=1,ii),j=1,jj)
            write(iunit,900)((y1+dy*real(j-1),i=1,ii),j=1,jj)
            write(iunit,'(30I4)')((mask(i,j),i=1,ii),j=1,jj)
            do m=1,nr
               write(iunit,900)((fld(i,j,m),i=1,ii),j=1,jj)
            enddo
         close(iunit)
         exit
      endif
   enddo
 900 format(10(1x,e14.7))
end subroutine tecfld2

subroutine tecfld3(fname,ii,jj,nr,fld,mask,xpos,ypos)
   implicit none
   character(len=*), intent(in) :: fname
   integer,          intent(in) :: ii,jj,nr
   real,             intent(in) :: fld(ii,jj,nr)
   integer,          intent(in) :: mask(ii,jj)
   real,             intent(in) :: xpos(ii,jj),ypos(ii,jj)
   integer m,i,j
   logical lopen
   integer iunit
   character(len=80) totfname
   integer ilen

   do iunit=10,20
      inquire(unit=iunit,opened=lopen)
      if (.not.lopen) then
!         totfname(:)=' '
!         totfname(1:4)='tec_'
!         ilen=len_trim(fname)
!         totfname(5:5+ilen-1)=fname(1:ilen)
!         totfname(5+ilen:5+ilen+3)='.dat'
!        open(iunit,file='tec_'//fname//'.dat',status='unknown')
         totfname(:)=' '
         totfname='tec_'//trim(fname)//'.dat'
         open(iunit,file=trim(totfname),status='unknown')
            write(iunit,*)'TITLE = "',fname,'"'
            write(iunit,*)'VARIABLES = "i-index" "j-index" "x" "y" "mask"'
            write(iunit,'(5(a,i3,a))')(' "F',i,'"',i=1,nr)
            write(iunit,'(a,i3,a,i3,a)')' ZONE  F=BLOCK, I=',ii,', J=',jj,', K=1'
            write(iunit,'(30I4)')((i,i=1,ii),j=1,jj)
            write(iunit,'(30I4)')((j,i=1,ii),j=1,jj)
            write(iunit,900)((xpos(i,j),i=1,ii),j=1,jj)
            write(iunit,900)((ypos(i,j),i=1,ii),j=1,jj)
            write(iunit,'(30I4)')((mask(i,j),i=1,ii),j=1,jj)
            do m=1,nr
               write(iunit,900)((fld(i,j,m),i=1,ii),j=1,jj)
            enddo
         close(iunit)
         exit
      endif
   enddo
 900 format(10(1x,e14.7))
end subroutine tecfld3


end module m_tecfld
