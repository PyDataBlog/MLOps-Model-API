function main()
	N = 20  +1 # num grid points = num squares +1

	# at border, only one path possible
	# init grid with what is known
	grid = zeros(eltype(N),N,N)
	for i = 1:N
		grid[i,N] = 1
		grid[N,i] = 1
	end

	# fill middle of grid
	for i = N-1:-1:1, j = N-1:-1:1
		grid[i,j] = grid[i+1,j] + grid[i,j+1]
	end

	println(grid)
	println(grid[1,1])
end

main()
