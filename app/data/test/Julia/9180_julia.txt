module DynamicArrays
using CUDArt

import Base: size, linearindexing, getindex, setindex!, similar, resize!, copy, copy!, pointer, reshape, isassigned
export DynamicArray, DynamicArrayCPU, DynamicArrayGPU

### Types and constructors

abstract DynamicArray{T,N} <: AbstractArray{T,N}
type DynamicArrayCPU{T,N} <: DynamicArray{T,N}; arr::Array{T,N}; ptr::Array{T,1}; end
type DynamicArrayGPU{T,N} <: DynamicArray{T,N}; arr::CudaArray{T,N}; ptr::CudaArray{T,1}; end

DynamicArray(a::Array)=DynamicArrayCPU(a)
DynamicArray(a::CudaArray)=DynamicArrayGPU(a)
DynamicArrayCPU(a::Array)=DynamicArrayCPU(a, reshape(a, length(a)))
DynamicArrayGPU(a::CudaArray)=DynamicArrayGPU(a, reshape(a, length(a)))
DynamicArray{S}(::Type{S}, dims::Dims)=DynamicArrayCPU(Array(S,dims))
DynamicArrayCPU{S}(::Type{S}, dims::Dims)=DynamicArrayCPU(Array(S,dims))
DynamicArrayGPU{S}(::Type{S}, dims::Dims)=DynamicArrayGPU(CudaArray(S,dims))

### Functions required for AbstractArrays from:
# http://julia.readthedocs.org/en/release-0.4/manual/interfaces
# So we get all the AbstractArray functions for free!

size(a::DynamicArray)=size(a.arr)
linearindexing(::DynamicArray)=Base.LinearFast()
getindex(a::DynamicArrayCPU,i::Int)=getindex(a.arr,i)
setindex!(a::DynamicArrayCPU,v,i::Int)=setindex!(a.arr,v,i)
# These are slow, use with caution, but I like looking into gpu arrays:
getindex{T}(a::DynamicArrayGPU{T},i::Int)=copy!(T[0],1,a.arr,i,1)[1]
setindex!{T}(a::DynamicArrayGPU{T},v,i::Int)=copy!(a.arr,i,T[convert(T,v)],1,1)
similar{S}(a::DynamicArrayCPU, ::Type{S}, dims::Dims)=DynamicArrayCPU(S,dims)
similar{S}(a::DynamicArrayGPU, ::Type{S}, dims::Dims)=DynamicArrayGPU(S,dims)


### Resizing: note that we always grow, never shrink.

# Resize factor: 1.3 ensures a3 can be written where a0+a1 used to be when a2 needs to grow
const resizefactor=1.3

function resize!(a::DynamicArray, d::Dims)
    size(a)==d && return a
    n = prod(d)
    n > length(a.ptr) && resize!(a.ptr, round(Int,resizefactor*n+1))
    a.arr = arr(a.ptr, d)
    return a
end

resize!(a::DynamicArray, d::Int...)=resize!(a,d)

# Based on Tim Holy's "sneaky workaround" suggested in https://goo.gl/mtvfz1
arr(a::Vector,d::Dims)=pointer_to_array(pointer(a), d)
arr(a::CudaVector,d::Dims)=CudaArray(a.ptr, d, a.dev)

### Resizing copy! -- it is not an error to copy! into a dynamic array of a different size
# The target resizes itself!
# It could even change its ndims, but that is part of the type signature so we don't want that.

typealias BaseArray{T,N} Union{Array{T,N},CudaArray{T,N}}
pointer(a::DynamicArray) = pointer(a.arr)

copy!{T,N}(a::DynamicArray{T,N}, b::BaseArray{T,N})=(resize!(a, size(b)); copy!(pointer(a), 1, pointer(b), 1, length(b)); a)
copy!{T,N}(a::DynamicArray{T,N}, b::DynamicArray{T,N})=copy!(a,b.arr)
copy!{T,N}(a::BaseArray{T,N}, b::DynamicArray{T,N})=(size(a)==size(b) ? copy!(pointer(a), 1, pointer(b), 1, length(b)) : throw(BoundsError()); a)

### Some missing stuff for CudaArrays:

# Generalizing low level copy using linear indexing to/from gpu arrays:

typealias BasePtr{T} Union{Ptr{T},CudaPtr{T}}

function copy!{T<:Real}(dst::BasePtr{T}, di::Integer, 
                        src::BasePtr{T}, si::Integer, 
                        n::Integer; stream=null_stream)
    esize = sizeof(T)
    nbytes = n * esize
    dptr = dst + (di-1) * esize
    sptr = src + (si-1) * esize
    CUDArt.rt.cudaMemcpyAsync(dptr, sptr, nbytes, CUDArt.cudamemcpykind(dst, src), stream)
    return dst
end

reshape(a::CudaArray, dims::Int...)=reshape(a, dims)
reshape(a::CudaArray, dims::Dims)=reinterpret(eltype(a), a, dims)

# TODO: define fast fill! for gpu
# TODO: colops and other things I defined for KUdense.
# TODO: test script

end # module
