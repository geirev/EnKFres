module m_prodhist
contains
subroutine prodhist(ctmp,days,nrdays,dirname,ensname)
! computes production history from ensemble diagnostics files
   use mod_states
   use mod_localdefs
   use mod_wellnames
   use m_ensio
   use m_get_ensemble

   implicit none

   integer,          intent(in) :: nrdays
   character(len=4), intent(in) :: ctmp(nrdays)
   real,             intent(in) :: days(nrdays)
   character(len=*), intent(in) :: dirname
   character(len=*), intent(in) :: ensname

   character(len=4) cnum
   character(len=1) cvar

   type(states4)  mem4
   type(states)   mem
   logical ex

   integer i,j,k,l,m
   integer nrvars,itime

   real day

   integer iflg,num,ivar,nrrec

   character(len=80) filename
   character(len=80) varname(1000)
   character(len=4)  var(6)


   character(len=9) rident
   character(len=4) ctime

   integer ierr,ird,iens,lwork,nact,nrobs


! Names of variables in tecplot file
   var(1)='WBHP'
   var(2)='WOPR'
   var(3)='WGPR'
   var(4)='WWPR'
   var(5)='WWCT'
   var(6)='WGOR'

   ivar=0
   do i=1,nrwells
      do j=1,6
         ivar=ivar+1
         varname(ivar)='"'//trim(wells(i))//':'//var(j)//'"'
      enddo
   enddo
   nrvars=ivar


! Counting number of records
   nrrec=0
   do num=initime,fintime
      write(cnum,'(i4.4)')num

      filename(:)=' '
      do j=1,2
         if (j==1) then
            cvar='F'
         else
            cvar='A'
         endif
         filename=trim(dirname)//trim(ensname)//cvar//cnum//'.uf'
         inquire(file=trim(filename),exist=ex)
         if (ex) nrrec=nrrec+1
      enddo
   enddo


! Dumping prodhist.dat file

   open(12,file='Prodhist/'//trim(ensname)//'.dat',access='sequential')
   write(12,'(a)')'TITLE="Prodhist results"'
   write(12,'(a)',advance='no')'VARIABLES='
   write(12,'(5a)',advance='no')'"time"'
   do m=1,nrvars
      write(12,'(1x,a)',advance='no')trim(varname(m))
   enddo
   write(12,'(a)')''
   write(12,'(a,i4,a)')'ZONE I=',nrrec,' F=POINT'

   do num=initime,fintime
      write(cnum,'(i4.4)')num

      filename(:)=' '
      do j=1,2
         if (j==1) then
            cvar='F'
         else
            cvar='A'
         endif

         filename=trim(dirname)//trim(ensname)//cvar//cnum//'.uf'
         inquire(file=trim(filename),exist=ex)
         if (ex) then
            print *,'prodhist: processing file: ',trim(filename)
            iflg=open_ensemblefile(trim(filename),'old')
            iflg=read_ensmem(1,mem4)
            mem=mem4

            do i=1,nrdays
               if (cnum==ctmp(i)) then
                  day=days(i)
                  exit
               endif
            enddo

            write(12,'(1x,f10.2)',advance='no')day
            do i=1,nrwells
               write(12,'((1x,e12.5))',advance='no')mem%BHP(i)
               write(12,'((1x,e12.5))',advance='no')mem%OPR(i)
               write(12,'((1x,e12.5))',advance='no')mem%GPR(i)
               write(12,'((1x,e12.5))',advance='no')mem%WPR(i)
               write(12,'((1x,e12.5))',advance='no')mem%WCT(i)
               write(12,'((1x,e12.5))',advance='no')mem%GOR(i)
            enddo
            write(12,'(1x)')
         endif
      enddo
   enddo
   close(12)

end subroutine
end module

