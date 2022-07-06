function doIndexMatch(t1, t2)
    if #t1 ~= #t2 then
        return false
    end
    for k, v in ipairs(t1) do
        if t2[k] ~= v then
            return false
        end
    end
    return true
end

function genColumn()
    local column = {}
    for i = 1, 3 do
        math.randomseed(os.time())
        column[i] = math.random() >= 0.5
    end
    return column
end

function getCombinations()
    local combinations = {}
    for i = 1, 511 do
        local editI = i
        for j = 9, 1, -1 do
            local magic = math.pow(2, i - 1)
            if editI >= magic then
                combinations[i][j] = true
                editI = editI - magic
            else
                combinations[i][j] = false
            end
        end
    end
    return combinations
end

local combos = getCombinations()

for i = 1, 5 do
    for j = 1, 3 do
        local printStr = ""
        for k = 1, 3 do
            local adder = ""
            printStr = printStr + 
