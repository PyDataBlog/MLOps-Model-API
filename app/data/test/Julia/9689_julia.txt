function sistema(x0,y0,w,N) # x0 ed y0 sono rispettivamente posizione e velocità iniziali, w è la pulsazione ed N è il grado di precisione

	a=Array{Float64}(N+1) # Vettore di appoggio per la posizione x
	b=Array{Float64}(N+1) # Vettore di appoggio per la posizione y

	a[1]=x0
	b[1]=y0
	T=2*pi/w       # T è il periodo delle soluzioni
	dt=T/N         # dt è l'intervallo di tempo tra due istanti successivi nella nostra approssimazione

	for k in 1:N
		a[k+1]=a[k]+b[k]*dt       # Formula di Taylor: x(t+dt)=x(t)+x'(t)dt=x(t)+y(t)dt
		b[k+1]=b[k]-w^2*a[k]*dt   # Formula di Taylor: y(t+dt)=y(t)+y'(t)dt=x(t)-(w^2)x(t)dt
	end

# Le funzioni di ritorno saranno interpolazioni lineari basate sui vettori di appoggio:
	function x(t) 
		t_modulo = Float64 # t_modulo sarà il valore di t riscalato nell'intervallo [0,T]
		if(t>=0)
			t_modulo = t % T
		else
			t_modulo = (t % T) + T
		end
		n = floor(Int64, t_modulo/(T/N) ) # n è l'indice di approssimazione
		if n==0
			return a[1]
		elseif n==N
			return a[N+1]
		else
			return a[n+1]+(a[n+2]-a[n+1])/(T/N)*(t_modulo-(n+1)T/N)
		end
	end	

	return x

end
