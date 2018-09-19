module m_sm_condiniensemble
contains
subroutine sm_condiniensemble
use mod_localdefs
use m_poroobs
use m_enkf
implicit none
character(len=1) yn
character(len=1) cvar
character(len=4) cnum
character(len=2) comm
character(len=80) ensfile
character(len=100) command
character(len=100) obsfile
logical ex
integer i,j,iflag,iopt
integer iens

character(len=80) eclname
character(len=80) ensname
character(len=80) ensdir
character(len=80) ecldir

logical :: interactive=.false.

if (lfacies) then
   print '(a)','Conditioning already done in iniens when lfacies=true.'
   return
endif  

inquire(file='poroobs.dat',exist=ex)
if (.not.ex) then
   print '(a)','poroobs.dat containing well logs of porosity does not exist.'
   print '(a)','poroobs.dat is of the format: '
   print '(a)','   i   j   k   porosity'
   print '(a)','  14  50   3   0.152338'
   print '(a)','format(3i4,f11.6)'
   return
endif

command(:)=' '
command='rm -f '//trim(ensbase)//'A0000.uf'
call system(trim(command))
call system('rm -f observations.uf')

call poroobs()

obsfile(:)=' '
obsfile='observations.uf'
call enkf(ensbase,'0000',trim(obsfile),lenkfana,logperm,.true.)

end subroutine
end module
