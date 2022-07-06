function add (a,b)
    if b >= 0
        for i = 1:b
            a += 1
        end
    else
        for i = b:-1
            a -= 1
        end
    end
    return a
end

