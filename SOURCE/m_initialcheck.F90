module m_initialcheck
contains
subroutine initialcheck()

logical ex

inquire(file='Diag',exist=ex)
if (.not.ex) call system('mkdir Diag')

inquire(file='Ensstat',exist=ex)
if (.not.ex) call system('mkdir Ensstat')

end subroutine
end module m_initialcheck
