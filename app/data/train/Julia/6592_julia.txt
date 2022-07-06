function Euler2()

	Fa = 1;
	Fb = 1;

	sum = 0;

	F = 0;

	while F<4e6
		F = Fa+Fb;
		Fa = Fb;
		Fb = F;
	
		if mod(F,2) == 0
			sum+=F
		end
	end
	

	return sum

end

@time Euler2()
println(Euler2())	
	
	