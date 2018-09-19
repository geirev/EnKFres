module m_printhelp
contains
subroutine printhelp(helpfile)
use mod_localdefs
implicit none
character(len=*), intent(in) :: helpfile
character(len=100) command
integer i,j
logical ex

command(:)=' '
command(1:5)='more '
i=len_trim(helpdir)
command(6:6+i-1)=trim(helpdir)

j=len_trim(helpfile)
command(6+i:6+i+j-1)=trim(helpfile)
print *,'+++',trim(command),'+++'

j=len_trim(command)
inquire(file=command(6:j),exist=ex)

if (ex) then
   call system(trim(command))
else
   print *,'Cannot find helpfile :', command(6:j)
   print *,'Check paths in mod_localdefs and helpfile directory.'
endif

end subroutine
end module
