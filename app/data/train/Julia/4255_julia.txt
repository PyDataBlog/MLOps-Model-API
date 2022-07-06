module UTime

export ut, gmt, localtime, UT, LCL, utc

import Base: show, string, (==), (!=)

using Base.Dates

using Compat


include("portable/CTime.jl")
include("portable/LCLandUTM.jl")

end # module
