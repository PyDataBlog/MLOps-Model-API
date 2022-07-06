function Primes(N::Int64)

	isprime = ones(Bool,N)
	
	for i = 2:int64(sqrt(N))
		if isprime[i]
			for j in (i*i):i:N
				isprime[j] = false
			end
		end
	end
	return filter(x -> isprime[x], 2:N)
end

function TrialDivision(N::Int64)
	
	primeFactors = [1]
	
	testPrimes = Primes(N);
	Nc = N;
	idx = 1;
	while (prod(primeFactors)!=N) && (idx < length(testPrimes))
		if mod(Nc, testPrimes[idx])==0
			println(testPrimes[idx])
			primeFactors=[primeFactors,testPrimes[idx]]
			Nc = Nc/testPrimes[idx]
		end
		idx = idx+1
	end
	
	return primeFactors[2:end]
end

function LargestPrimeFactor(N::Int64)
	
	first20 = Primes(1000)
	largest = 1
	for p in first20
		if mod(N, p) == 0
			largest = p
			N = fld(N,p)
		end
	end
	if N == 1
		return largest
	end
	trials = flipud(Primes(N+1))
	for p in trials
		if (mod(N,p) == 0)&&(p > largest)
			println(p)
			largest = p
			break
		end
	end
	if largest == 1
		return N
	else
		return largest
	end
end
	
function Euler3(N::Int64)
	
	big = LargestPrimeFactor(N)
	println("The largest prime factor of ",N," is ",big,".")
	
end

@time Euler3(600851475143)	
#Euler3(100)
	