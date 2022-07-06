# subtract one unit to the power limit
# SP-
export psp_decrementplimit

const DECREMENT_PLIMIT = [0x53, 0x50, 0x2D, 0x0D]

"""
    psp_decrementplimit(io)
    
Subtract one unit to the power limit.
"""
function psp_decrementplimit(io_psp::IO)
  write(io_psp, DECREMENT_PLIMIT)
  return nothing
end
