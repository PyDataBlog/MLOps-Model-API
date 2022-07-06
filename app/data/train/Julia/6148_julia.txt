"Fourth root (\\fourthroot)"
∜x = x^(1/4)

"Real part of complex number (\\Re)"
const ℜ = real
"Imaginary part of complex number (\\Im)"
const ℑ = imag

"""
Angle (\\angle).

For complex numbers returns the phase angle, for reals \$∠(ω) = e^{iπω}\$.

```julia
julia> ∠(∠(∠(∠(2))))
2.0
```

Parentheses are necessary.
"""
∠(z::Complex) = angle(z)
∠(ω::Real) = cis(ω)
@vectorize_1arg Complex ∠
@vectorize_1arg Real ∠

"Function composition (\\circ)"
function (f∘g)
    function (args...)
        f(g(args...))
    end
end

"Bitwise egation (\\neg)"
const ¬ = ~
"Bitwise and (\\wedge)"
const ∧ = &
"Bitwise or (\\vee)"
const ∨ = |
"Bitwise xor (\\oplus)"
const ⊕ = $

"Sigmoid function (\\mitS)"
𝑆(t) = 1/(1+exp(-t))
# TODO: maybe some library has a better implementation
@vectorize_1arg Number 𝑆

"Fast Fourier transform (\\mscrF)"
const ℱ = fft
"Inverse fast Fourier transform (\\mscrF\\^-\\^1)"
const ℱ⁻¹ = ifft
