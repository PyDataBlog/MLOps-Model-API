"""
A test for gage(GeometricCenter, obj_array).

Arguments
----------
:Type{GeometricCenter}
    must be the Julia Type object ``GeometricCenter``

array:AbstractArray
    a floating point array or array of arrays

solution:AbstractArray
    answer key

msg::AbstractString
    optional test message
"""
function test_gage(::Type{GeometricCenter},
    array::AbstractArray,
    solution::AbstractArray,
    msg::AbstractString)
    
    println("-----------------------------------")
    print_with_color(:blue, "Test gage(GeometricCenter, array)\n\n")
    if msg != ""
        print_with_color(:blue, "$(msg)\n")
    end

    @time answer = gage(GeometricCenter, array)

    @test_approx_eq answer solution

    
    println("-----------------------------------")
    print_with_color(:green, "VERIFIED! gage(GeometricCenter, array)\n")

    return true

end

include("unit_test_1_gage_array.jl")
include("unit_test_2_gage_array.jl")
include("unit_test_1_gage_array_of_arrays.jl")
include("unit_test_2_gage_array_of_arrays.jl")
