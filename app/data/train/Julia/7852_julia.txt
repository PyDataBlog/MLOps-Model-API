function Euler1(N::Int64)
	
	sum = 0;
	for i in 1:(N-1)
		if (mod(i,3)==0) || (mod(i,5)==0)
			sum+=i
			println(i)
		end
	end
	
	return sum
end

@time Euler1(1000)
println(Euler1(1000))