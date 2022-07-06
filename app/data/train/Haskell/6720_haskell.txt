
import qualified NS3473.Rebars as R
import qualified NS3473.Concrete as M
import qualified NS3473.Walls as W
import qualified NS3473.Buckling as X

rebar12 = R.Rebar 12

rebar10 = R.Rebar 10

rebar = R.DoubleWallRebars rebar12 100 rebar10 100 40

conc = M.newConc "35"

wall = W.Wall 200 2400 conc rebar W.External 1.0

ic = X.ic wall

minrar = W.minHorizRebars wall
