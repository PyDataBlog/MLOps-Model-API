# read the output plimit
export psp_readplimit

const READ_PLIMIT = [0x50, 0x0d]

"""
    psp_readplimit(io)

Read the power limit.
"""
function psp_readplimit(io_psp::IO)
  write(io_psp, READ_PLIMIT)
  plimitstring = bytestring(read(io_psp,UInt8, 7))
  if  uppercase(plimitstring[1]) != 'P' ||
      plimitstring[5:7] != "\r\r\n"
        error("PSP Invalid Response: ",plimitstring)
  end
  parse(Float64,plimitstring[2:4])
end
