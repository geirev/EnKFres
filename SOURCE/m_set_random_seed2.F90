module m_set_random_seed2
contains
subroutine set_random_seed2
! Sets a random seed based on the system and wall clock time
   implicit none 

   integer , dimension(8)::val
   integer clock
   integer n,i
   integer, allocatable, dimension(:):: seed

   call DATE_AND_TIME(values=val)
   call SYSTEM_CLOCK(count=clock)
   call RANDOM_SEED(size=n)
   allocate(seed(n))
   seed=clock + 37 * (/ (i - 1, i = 1, n) /)
   print *,'seed:',seed
   print *,'val',val
   call RANDOM_SEED(put=seed)
   deallocate(seed)
end subroutine set_random_seed2
end module m_set_random_seed2
