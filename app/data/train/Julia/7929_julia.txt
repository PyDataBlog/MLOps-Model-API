using cme257hw6
using Base.Test

function R(z::Complex)
    return z
end


@test cme257hw6.jset(R, collect(10:100), collect(0:100), 100, 10.) == zeros(91,101)
#this should be the zero matrix; everything immediately falls outside of the convergence theshhold

@test cme257hw6.jset(R, collect(10:100), collect(0:100), 100, 1000.) == 101*ones(91,101)
#everything begins in the convergence area, so everything stays convergent.


