module GGPlot

export
    ggplot,
    aes,
    geom_point,
    geom_line

using DataFrames
using GR

abstract AbstractLayer
abstract AbstractGeom <: AbstractLayer

include("aes.jl")
include("plot.jl")
include("geom_point.jl")
include("geom_line.jl")
include("gr_backend.jl")

end # module
