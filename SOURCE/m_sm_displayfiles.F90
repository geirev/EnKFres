module m_sm_displayfiles
contains
subroutine sm_displayfiles()
character(len=2) comm

comm=' '
do 
   print '(a)','    ===================================================='
   print '(a)','    =   a: more wells.dat    (aa or v in more to edit) ='
   print '(a)','    =   b: more prepobs.dat  (bb or v in more to edit) ='
   print '(a)','    =   c: more days.dat     (cc or v in more to edit) ='
   print '(a)','    =   q: return to main menu                         ='
   print '(a)','    ===================================================='
   write(*,'(a)',advance='no')'Command: '
   read(*,*)comm

   select case (trim(comm))
   case('q')
      exit
   case('a')
      call system('more wells.dat')
   case('aa')
      call system('vi wells.dat')
   case('b')
      call system('more prepobs.def')
   case('bb')
      call system('vi prepobs.def')
   case('c')
      call system('more days.dat')
   case('cc')
      call system('vi days.dat')
   case default
      print '(a)','Invalid command: try again!'
   end select

enddo

end subroutine
end module
