! A 'implied' do loop
program doimplied

    implicit none

    integer                 :: i
    integer, dimension(5)   :: P

    P = (/ (i**2, i=1,5) /)
    write(*,'(5I3)') P

end program doimplied
