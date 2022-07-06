type うま味{T<:Real}
    concentration :: T
end

issavory(A) = false
issavory(A::うま味) = A.concentration > 0

#Hardware support for the Mac Tastebud Sensor
if OS_NAME == :Darwin
    if !isfile("lmu.dylib")
        #Build the Mac Tastebud Sensor interface
         cd(joinpath(Pkg.dir("Taste"), "src"))
         run(`make`)
     end

    measureumami() = うま味(ccall((:readlmu, :lmu), Int, ()) * eps()^0.25)
else
    measureumami() = error("MacOS X required for Mac Umami Sensor functionality")
end
