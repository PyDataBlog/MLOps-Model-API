program main

  ! A named common block here is implicitly saved.
  common /factors/ sv

  real(kind = 8), dimension(3) :: sv
  integer :: i

  write(*, *) "setting factors..."
  do i = 1, 3
    sv(i) = i
  enddo

  call set_data()
  call print_factors()
  call another_print()

  write(*, *) "done here"

end program main
