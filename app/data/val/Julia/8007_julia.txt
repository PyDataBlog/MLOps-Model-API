# Tested on Julia 0.3

macro swap!(x, y)
    quote
        tmp = $(esc(x))
        $(esc(x)) = $(esc(y))
        $(esc(y)) = tmp
    end
end

a = 1
b = 2

@swap!(a, b)

println("a: $a, b: $b")
