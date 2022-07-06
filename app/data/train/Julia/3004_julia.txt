function numdivisors(n)
	d = 0
	limit = n;
    
    i = 1
	while i < limit
		if n % i == 0
			limit = div(n,i) # converge to sqrt(n)
			if limit != i
				d += 1
			end
			d += 1
		end
		i += 1
	end
	return d
end

function main()
	N = 5

	n = 0
	i = 0
	while true
		i += 1
		n += i
		d = numdivisors(n)
		println("$d $n")
		if d > N
			println(n)
			return
		end
	end
end

main()
