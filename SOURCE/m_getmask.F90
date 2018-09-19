module m_getmask
use mod_dimensions
integer, public :: mask(nx,ny,nz)
logical, public :: lmask(nx,ny,nz)
logical, public :: maskread=.false.
contains
subroutine getmask()
   use mod_states
   use m_fegrid
   integer i,j,k,iactive
   logical ex

! read mask for model grid
   inquire(file='mask.uf',exist=ex)
   if (ex) then
      open(10,file='mask.uf',form='unformatted')
         read(10)i,j,k
         if (i==nx .and. j==ny .and. k==nz) then
            read(10)mask
            print '(a)','getmask: mask read from mask.uf'
            iactive=sum(mask(:,:,:))
            maskread=.true.
            if (iactive /= nactive) then
               print '(a,i7,a,i7)','getmask: number of active grid points in mask.uf =',iactive,&
                       ' is not equal to nactive=',nactive
               call system('rm -f mask.uf')
               ex=.false.
               maskread=.false.
            endif
         else
            print '(a)','getmask: mask.uf not up to date will create a new one.'
            call system('rm -f mask.uf')
            ex=.false.
         endif
      close(10)
   endif
   
   if (.not.ex) then
      call fegrid(mask,maskread)
      print *,'fegrid done'
      iactive=sum(mask(:,:,:))
      if (maskread .and. iactive /= nactive) then
         print *,'number of active grid points in mask.uf =',iactive,' as generated from the'
         print *,'Refcase/ECLIPSE.FEGRID file is not equal to nactive=',nactive
         print *,'You either have a wrong definition of nactive in mod_dimensions.F90, or'
         print *,'the Refcase/ECLIPSE.FEGRID file is too old or new :-) '
         stop
      endif
   endif

   if (maskread) then
      lmask=.false.
      where (mask==1) lmask=.true.
   endif

end subroutine getmask
end module m_getmask
