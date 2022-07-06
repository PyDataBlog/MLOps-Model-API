using PyPlot

# debug
function say(sth)
  println(sth)
end

XI100 = 124.342
XI80 = 101.879

function test_a(;verbose=true)
  n = 10000
  bins = 101

  U = rand(n)

  (range, counts) = hist(U, 0:1/bins:1)

  D = sum((counts - n/bins).^2/(n/bins))

  verbose ? say("n=$n, bins=$bins, D=$(round(D*1000)/1000) $(D>XI100?"> $XI100, Reject H0, bad random":"< $XI100, Fail to reject H0, good random")") : nothing

  D
end

# dist
function chi_square(x, nu)
  x.^(nu/2-1).*exp(-x/2)./(2^(nu/2)*gamma(nu/2))
end

function test_c(;verbose=true)
  n = 10000
  k0 = 9
  bins = k0^2
  ind(x) = floor(x*9)

  U = rand(n,2)

  Data = [Float64(k0 * ind(U[i,1]) + ind(U[i,2])) for i=1:n]

  (range, counts) = hist(Data[:], -1:80)

  D = sum((counts - n/bins).^2/(n/bins))

  verbose ? say("n=$n, k0=$k0, bins=$bins, D=$(round(D*1000)/1000) $(D>XI80?"> $XI80, Reject H0, bad random":"< $XI80, Fail to reject H0, good random")") : nothing

  D
end
