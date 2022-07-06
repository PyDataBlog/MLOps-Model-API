"""
Return a vector with one less dimension 
with i-th item removed.

Arguments
----------
input:Array
    input vector 

i:Integer
    which item to remove   
"""
function smaller_vector(input::Array, i::Integer)
    return vcat(input[1:i-1],input[i+1:end])
end