module m_sm_swapmem
contains
subroutine sm_swapmem()
   use m_swapmem
   implicit none
   character(len=1) yn
   logical ex

   print '(a)','Swapmem allows the user to replace one ensemble member with anonter.'
   print '(a)','This can be useful if one member fails to integrate or is unstable.'
   print '(a)','Note that this reduces the rank of the ensemble.'

   write(*,'(a)',advance='no')'Do you still want to run swapmem (y/n): '
   read(*,*)yn
   if (yn =='y') then
      call swapmem()
   endif

end subroutine
end module
