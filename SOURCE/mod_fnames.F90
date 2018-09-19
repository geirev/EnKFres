module mod_fnames
   character(len=80) ensFfile
   character(len=80) ensAfile
   character(len=80) ensSfile
   character(len=80) eclFfile
   character(len=80) eclAfile
   character(len=80) eclHfile

   character(len=80) ensXfile

   contains

   subroutine fnames(ensdir,ensbase,ecldir,eclbase,cvar,cnum)
   character(len=*), intent(in) :: eclbase
   character(len=*), intent(in) :: ensbase
   character(len=*), intent(in) :: ecldir
   character(len=*), intent(in) :: ensdir
   character(len=1), intent(in) :: cvar
   character(len=4), intent(in) :: cnum
   integer ilen,jlen

   open(10,file='fnames.log')

   jlen=len_trim(ensdir)
   ilen=len_trim(ensbase)

   ensAfile=' '
   ensAfile(1          : jlen        )=ensdir(1:jlen)
   ensAfile(jlen+1     : jlen+ilen   )=ensbase(1:ilen)
   ensAfile(jlen+ilen+1: jlen+ilen+1 )='A'
   ensAfile(jlen+ilen+2: jlen+ilen+5 )=cnum(1:4)
   ensAfile(jlen+ilen+6: jlen+ilen+8 )='.uf'
   write(10,'(2a)')'analysis ensemble file is: ',trim(ensAfile)

   ensFfile=' '
   ensFfile(1          : jlen        )=ensdir(1:jlen)
   ensFfile(jlen+1     : jlen+ilen   )=ensbase(1:ilen)
   ensFfile(jlen+ilen+1: jlen+ilen+1 )='F'
   ensFfile(jlen+ilen+2: jlen+ilen+5 )=cnum(1:4)
   ensFfile(jlen+ilen+6: jlen+ilen+8 )='.uf'
   write(10,'(2a)')'forecast ensemble file is: ',trim(ensFfile)

   ensSfile=' '
   ensSfile(1          : jlen        )=ensdir(1:jlen)
   ensSfile(jlen+1     : jlen+ilen   )=ensbase(1:ilen)
   ensSfile(jlen+ilen+1: jlen+ilen+1 )='S'
   ensSfile(jlen+ilen+2: jlen+ilen+5 )=cnum(1:4)
   ensSfile(jlen+ilen+6: jlen+ilen+8 )='.uf'
   write(10,'(2a)')'static   ensemble file is: ',trim(ensSfile)

   if (cvar=='A') then
      ensXfile=ensAfile
   endif

   if (cvar=='F') then
      ensXfile=ensFfile
   endif


   jlen=len_trim(ecldir)
   ilen=len_trim(eclbase)
   eclAfile=' '
   eclAfile(1           : jlen        )=ecldir(1:jlen)
   eclAfile(jlen+1      : jlen+ilen   )=eclbase(1:ilen)
   eclAfile(jlen+ilen+1 : jlen+ilen+2 )='.A'
   eclAfile(jlen+ilen+3 : jlen+ilen+6 )=cnum(1:4)
   write(10,'(2a)')'Eclipse summary file is  : ',trim(eclAfile)

   eclFfile=' '
   eclFfile(1           : jlen        )=ecldir(1:jlen)
   eclFfile(jlen+1      : jlen+ilen   )=eclbase(1:ilen)
   eclFfile(jlen+ilen+1 : jlen+ilen+2 )='.F'
   eclFfile(jlen+ilen+3 : jlen+ilen+6 )=cnum(1:4)
   write(10,'(2a)')'Eclipse restart file is  : ',trim(eclFfile)

   eclHfile=' '
   eclHfile(1           : jlen        )=ecldir(1:jlen)
   eclHfile(jlen+1      : jlen+ilen   )=eclbase(1:ilen)
   eclHfile(jlen+ilen+1 : jlen+ilen+8 )='.FSMSPEC'
   write(10,'(2a)')'Eclipse summary header is: ',trim(eclHfile)

   end subroutine fnames
   end module mod_fnames
