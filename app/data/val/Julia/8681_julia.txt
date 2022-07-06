"""
Return the principal axes of the rigid body
defined by the input coordinates.
The order of the principals axes is sorted
by the eigenvalues of the moment of inertia matrix
(from low to high).
Therefore the first column of the returned 
principal axes matrix is the direction 
along which the object is the most elongated.

Arguments
-----------
:Type{PrincipalAxes}
    must be type `PrincipalAxes`

coordinates:AbstractArray
    must be a 2D matrix and must have exactly 3 columns

center=[]:AbstractArray
    (keyword) center of rotation
"""
function gage(::Type{PrincipalAxes},coordinates::AbstractArray; center::AbstractArray=[])
    if length(coordinates) == 0
        return Array{AbstractFloat,1}()
    end


    # If input coordinates is an array of array
    # then convert it to a 2D matrix
    if issubtype(typeof(coordinates[1]), Array)
        coordinates = vcat([x' for x in coordinates]...)
    end

    # change rotation center
    if length(center) == 3
        coordinates = coordinates .- transpose(center)
    end

    shape = size(coordinates)
    @debug @assert length(shape) == 2
    @debug @assert shape[2] == 3

    I = zeros(3,3) # moment of inertia tensor
    I[1,1] = sum(coordinates[:,2].^2 + coordinates[:,3].^2)
    I[2,2] = sum(coordinates[:,1].^2 + coordinates[:,3].^2)
    I[3,3] = sum(coordinates[:,1].^2 + coordinates[:,2].^2)
    I[1,2] = -sum(coordinates[:,1] .* coordinates[:,2])
    I[1,3] = -sum(coordinates[:,1] .* coordinates[:,3])
    I[2,3] = -sum(coordinates[:,2] .* coordinates[:,3])
    I[2,1] = I[1,2]
    I[3,1] = I[1,3]
    I[3,2] = I[2,3]
    D,V = eig(I)
    return V[:,sortperm(D)] # sort the eigenvectors by the eigenvalues
end