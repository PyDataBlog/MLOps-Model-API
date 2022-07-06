module cme257Julia
export cme257Julia_Sets

abstract cme257function

type cme257Julia_Sets{T} <: cme257function
    x::AbstractArray{T,1}
    y::AbstractArray{T,1}
    R::Function
    n_iter::Int64
    escape_tol::Int64
end

function cme257Julia_Sets{T}(x::AbstractArray{T,1}, y::AbstractArray{T,1}, R::Function, n_iter::Int64, escape_tol::Int64)
    A = zeros(length(x), length(y))
    for i = 1:length(x)
        for j = 1:length(y)
            z = Complex{T}(x[i] + im * y[j])
            for k = 1:n_iter
                z = Complex{T}(R(z))
                if abs(z) > escape_tol
                    A[i,j] = k
                    break
                end
            end
            if A[i,j] == 0
                A[i,j] = escape_tol + 1
            end
        end
    end
    return A
end

end
