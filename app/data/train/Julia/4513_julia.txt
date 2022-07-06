function isdivisiblebyall(n, tab)
	for v in tab
		if n % v != 0
			return false
		end
	end
	return true
end

import Base.factorize
function factorize(n,F)
	f = zeros(eltype(n), F)
	f[1] = 1
	i = n
	while i > 1	
		if isprime(i) && n % i == 0
			f[i] += 1
			n = div(n,i)
			i = n
		else
			i -= 1
		end
	end
	return f
end

function main()
	N = 20
	f = zeros(eltype(N), N, N)
	for i = N:-1:1
		f[:,i] = factorize(i, N)
	end
	println(f)
	resf = maximum(f, 2)
	println(resf')
	n = 1
	for i = 1:N
		for j = 1:resf[i]
			n *= i
		end
	end
	println(n)
	assert(isdivisiblebyall(n, [1:N]))
end

main()
