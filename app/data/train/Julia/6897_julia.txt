module Sandbox

# package code goes here

module Sand
    export f
    f(x) = x^2
end

using .Sand
include(joinpath("submodule", "submodule.jl"))
using .SubModule
export f, g

end # module
