"""
A test for gage(ProjectedRotationAngle,...).

Arguments
----------
:Type{ProjectedRotationAngle}
    must be type ``ProjectedRotationAngle``

input:AbstractArray
    the vector to be rotated

target:AbstractArray 
    the target vector to be aligned to

ref_axis:AbstractArray
    the reference axis around which input vector will be rotated

solution:AbstractFloat
    answer key 
    
msg="":AbstractString
    optional test message

tol_near_zero=1e-15:AbstractFloat
    (optional) tolerance for the length of the input vectors to be close to zero
"""
function test_gage(::Type{ProjectedRotationAngle}, 
    input::AbstractArray, target::AbstractArray, ref_axis::AbstractArray, 
    solution::AbstractFloat, msg::AbstractString="", tol_near_zero::AbstractFloat=1e-15)
    print_dashed_line(80)
    print_with_color(:blue, "Test gage(ProjectedRotationAngle,...)\n\n")
    if msg != ""
        print_with_color(:blue, "$(msg)\n\n")
    end

    @time answer = gage(ProjectedRotationAngle, input, target, ref_axis, tol_near_zero)
    @test_approx_eq answer solution

    print_with_color(:green, "VERIFIED!\n")
    print_dashed_line(80)
    return true
end

test_gage(ProjectedRotationAngle, [1.,0.,0.], [0.,1.,0.], [0., 0., 2.], pi/2,"test 1")
test_gage(ProjectedRotationAngle, [1.,0.,0.], [0.,0.,1.], [0., 1., 0.], pi/2,"test 2")
test_gage(ProjectedRotationAngle, [1./sqrt(2),1./sqrt(2),1.], [0.,0.,1.], [1., -1., 0.], pi/4,"test 3")
