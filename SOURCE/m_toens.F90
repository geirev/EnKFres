module m_toens
contains
subroutine toens()
use mod_localdefs
use m_interface
implicit none

character(len=80) eclname
character(len=80) ensname
character(len=80) ensdir
character(len=80) ecldir
character(len=4)  cnum
character(len=1)  cvar
character(len=80) filename
integer i,j,iopt,iens,inum
logical interactive

integer :: wellsok=0
integer :: backtostart=0
character(len=1) yn
logical ex

call system('rm -f days.dat')

inum=initime
do 
   write(cnum,'(i4.4)')inum
   print *,'toens: inum=',inum

! write to dir: refdir
   ecldir=refdir

! write to file
   ensname(:)=' '
   ensname(1:7)='refcase'

! read from dir: refdir
   ensdir=refdir

! read from file:  eclbase
   eclname=eclbase

   cvar='F'

! Store as ensemble member 1
   iens=1

! Option: convert from ecl to ens
   iopt=1

   filename(:)=' '
   filename=trim(ensdir)//trim(eclname)//'.F'//trim(cnum)
   inquire(file=trim(filename),exist=ex)
   if (ex) then
      call interface(ensdir,ensname,ecldir,eclname,cvar,cnum,iens,iopt)
   else
      print *,'toens: File ',trim(filename),' does not exist. Exits loop'
      exit
   endif


   if (inum == 1 .and. wellsok == 0) then
      print *
      print *,'*********************************************************************************************'
      print *,' IMPORTANT NOTICE!'
      print *,' You may edit the file wells.dat and remove lines not associated with real wells'
      print *,' from which you are assimilating data.  The remaining lines in wells.dat define the'
      print *,' wells and associated variables stored in the states variable (see mod_states.F90). Further '
      print *,' the order of the lines defines the order these are stored in the state variable.'
      print *,' NB: After this file has been defined you should not change it again during an experiment.'
      print *,'*********************************************************************************************'
      print *
      print *,'press RETURN to continue: '
      read(*,*)
 
      print *,'wells.dat : '
      print *,' *********************************************************************************************'
      inquire(file='wells.dat',exist=ex)
      if (ex) then
         call system('cat wells.dat')
      else
         print *,'ERROR (toens) : wells.dat does not exist'
      endif
      print *,' *********************************************************************************************'
      write(*,'(a)',advance='no')'Do you want to edit wells.dat (y/n) ? '
      read(*,*)yn

      if (yn=='y') then
         call system('vi wells.dat')
         wellsok=1
         inum=-1
      endif
   endif

   inum=inum+1

   if (inum==fintime) exit
enddo

end subroutine
end module
