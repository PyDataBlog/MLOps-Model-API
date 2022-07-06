module libfft
	use mylib
	CONTAINS
subroutine dft(input_file, x_fft)
    implicit none
	! dummy vars
	character (len=30) :: input_file
	complex, dimension(:), allocatable :: x_fft
	
	! local vars
	real, parameter :: PI = 4*atan(1.0)
    real, dimension(:), allocatable :: x
    integer :: N, k, i
	complex :: A, x_fft_sum
    
    ! Find how many lines are in the file
    call number_of_lines(input_file, N)
	
	! Load data from input_file
    allocate(x(N))
    call loadArrayFromFile(input_file, N, x)
    
	! allocate memory for FFT array
    allocate(x_fft(N))
	
	! Main Loop
	do k=1,N
		A = exp(cmplx(0,-2*PI*k/N))
		x_fft_sum = 0
		do i=1,N
			x_fft_sum = x_fft_sum + x(i) * (A**i)
		enddo
		x_fft(k) = x_fft_sum
	enddo	

end subroutine

end module libfft