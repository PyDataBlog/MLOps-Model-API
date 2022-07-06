!
! Lattice paths
!
! Problem 15
!
! Starting in the top left corner of a 2×2 grid, and only being able to move
! to the right and down, there are exactly 6 routes to the bottom right corner.
!
! (fig.)
!
! How many such routes are there through a 20×20 grid?
!
program p_015
  use, intrinsic :: iso_fortran_env
  implicit none
  integer(int64) :: n, i, a
  n = 20
  a = 1
  do i = 1, n
     a = a * 2 * (i * 2 - 1) / i
  end do
  write(output_unit, '("solution: ",g0)') a
end program p_015
