-- receive arbitrary values, return all but first

function return_but_first(...)
    local vector = {}
    local i = 0

    for _, v in ipairs{...} do
        if i == 0 then
            i = i + 1
        else
            vector[i] = v
            i = i + 1
        end
    end
    return vector
end

