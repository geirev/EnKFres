module m_mkobsfile
contains
subroutine mkobsfile()
! Reads data from different data sources and converts the
! information to the parameters stored in observations.uf
! Additional information is needed about the model grid.

   use mod_localdefs
   use mod_eclsummary
   use mod_eclfsmspec
   use m_eclipse_read
   use m_eclipse_write
   implicit none

   integer i,m,ird,j
   integer ifile
   character(len=4) cfile
   character(len=80) command
   logical ex

! obs-FILE variables
   integer DATATIME(1)
   character(len=8)      :: obsname(1)='DATATIME'
   integer               :: obssize(1)=1
   character(len=4)      :: obstype(1)='INTE'
   character(len=80) totfname
   integer itmp 


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Read header file
#include "fsm_allocate.inc"

   totfname(:)=' '
   i=len_trim(refdir)
   j=len_trim(eclbase)
   totfname(1:i)=trim(refdir)
   totfname(i+1:i+j)=trim(eclbase)
   totfname(i+j+1:i+j+8)='.FSMSPEC'

   inquire(file=trim(totfname),exist=ex)
   if (.not.ex) then
      print *,'mkobsfile: ',trim(totfname),' does not exist'
      return
   endif
   open(10,file=trim(totfname))

   ihead=0; ikeywords=0; iwgnames=0; iunits=0
   do 
      ihead=ihead+1
      read(10,'(t3,a8,t13,i11,t26,a4)',end=997)headname(ihead),headsize(ihead),headtype(ihead)
      print *,'HEADER: ',headname(ihead),headsize(ihead),headtype(ihead)
      select case (trim(headname(ihead)))
#include "fsm_readeclipse.inc"
      case default
         print *,'variable not contained in select in fegrid.F90 : ',headname(ihead)
         stop
      end select

   enddo
   997 close(10)

  print *,'summary header file read'

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Write header in observations.dat
   if (ikeywords ==0 .or. iwgnames==0 .or. iunits == 0) then
       print *,'ERROR: mkobsfile:  .FSMSPEC file does not contain all vars: ',ikeywords,iwgnames,iunits
       return
   endif

   open(10,file='observations.dat',status='unknown',form='formatted',access='sequential')
       i=ikeywords; ird=write_char(headname(i),headsize(i),headtype(i),KEYWORDS)
       i=iwgnames ; ird=write_char(headname(i),headsize(i),headtype(i),WGNAMES)
       i=iunits   ; ird=write_char(headname(i),headsize(i),headtype(i),UNITS)
   close(10)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Read Afiles and append to observation.dat
#include "sum_allocate.inc"

   ifile=0
   do
      ifile=ifile+1
      write(cfile,'(i4.4)')ifile

      print *,'mkobsfile: reads file = ',trim(eclbase),'.A',cfile
      totfname(:)=' '

      i=len_trim(refdir)
      j=len_trim(eclbase)
      totfname(1:i)=trim(refdir)
      totfname(i+1:i+j)=trim(eclbase)
      totfname(i+j+1:i+j+2)='.A'
      totfname(i+j+3:i+j+6)=cfile(1:4)

      inquire(file=trim(totfname),exist=ex)
      if (ex) then
         open(10,file=trim(totfname),form='formatted',access='sequential')
            isumm=1
            read(10,'(t3,a8,t13,i11,t26,a4)')summname(isumm),summsize(isumm),summtype(isumm)
            print *,'HEADER: ',summname(isumm),summsize(isumm),summtype(isumm)
            if (summname(isumm) /= 'SEQHDR') stop 'interface: SEQHDR'
            deallocate(SEQHDR); allocate(SEQHDR(summsize(isumm)))
            ird=read_integer(summsize(isumm),SEQHDR,summtype(isumm))

            do
               isumm=2
               read(10,'(t3,a8,t13,i11,t26,a4)',end=99)summname(isumm),summsize(isumm),summtype(isumm)
               print *,'HEADER: ',summname(isumm),summsize(isumm),summtype(isumm)
               if (summname(isumm) /= 'MINISTEP') stop 'interface: MINISTEP'
               deallocate(MINISTEP); allocate(MINISTEP(summsize(isumm)))
               ird=read_integer(summsize(isumm),MINISTEP,summtype(isumm))

               isumm=3
               read(10,'(t3,a8,t13,i11,t26,a4)')summname(isumm),summsize(isumm),summtype(isumm)
               print *,'HEADER: ',summname(isumm),summsize(isumm),summtype(isumm)
               if (summname(isumm) /= 'PARAMS') stop 'interface: PARAMS'
               deallocate(PARAMS); allocate(PARAMS(summsize(isumm)))
               ird=read_real(summsize(isumm),PARAMS,summtype(isumm))
            enddo
         99 close(10)

         open(10,file='observations.dat',position='append',form='formatted',access='sequential')
            print *,ifile
            DATATIME(1)=ifile
            i=1; ird=write_integer(obsname(i),obssize(i),obstype(i),DATATIME)
            i=3; ird=write_real(summname(i),summsize(i),summtype(i),PARAMS)
         close(10)
      else
         exit
      endif

   enddo

! Remove blank lines in file caused by the append 
   command='cat observations.dat | sed -e ''/^$/d'' > tmpobs '
   call system(trim(command))
   call system('mv tmpobs observations.dat')

   call fsmdeallocate()
   call sumdeallocate()

   print *,'mkobsfile: done...'

end subroutine
end module
