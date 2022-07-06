
module Go

export fix, exists,
       black, empty, white, other, point, Position, IllegalMove, move!,
       evaluate, random_expression, moves,
       evolve, evolve2, dump_expression, undump, 
       replay_direct, replay_pair, replay_latest,
       longest_lived, final_gen,
       parse_log, plot_tramlines, ancestors, multi, multig, children

using AutoHashEquals
using StatefulIterators
using SHA
import Drawing; D = Drawing
import ColorTypes; C = ColorTypes
using ColorVectorSpace
import Base.show, Base.delete!, Base.isless, Base.hash, Base.==
import FixedPointNumbers; N = FixedPointNumbers


# todo
# start on net / pre-processing
# graphs, evolution, tournaments etc

include("utils.jl")
include("emulation.jl")
include("position.jl")
include("expression.jl")
include("play.jl")
include("evolution.jl")
include("log.jl")

end
