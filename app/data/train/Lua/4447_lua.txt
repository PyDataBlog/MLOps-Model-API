function exchange(compName, side)
    local modem = nil
    if side then
        modem = peripheral.wrap(side)
    end
    if not modem then
        local modem = peripheral.find("modem")
    end
    modem.transmit(65535, os.getComputerID(), {
        type = "powerboat9_RDP",
        msg = "getServer",
        data = {
            name = compName
        }
    })
    local done = false
    while not done do
        local data = {pullEvent("modem_message")}
        if (type(data[4]) == "table") and (data[4].type == "powerboat9_RDP") and (data[4].msg == "tellServer") and data[4].data and (type(data[4].data) == "table")
