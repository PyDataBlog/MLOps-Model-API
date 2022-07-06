module MyInterpolations
function my_lin_interp(grid, vals)
    function func(x::Real)
        if x < grid[1]
            print("error")
        end
        if x > grid[end]
            print("error")
        end
        if x == grid[1]
            return vals[1]
        end
        if grid[1] < x <= grid[end]
        i = searchsortedfirst(grid, x)
        return vals[i-1] + (vals[i]-vals[i-1])*(x - grid[i-1])/(grid[i] - grid[i-1])
        end
    end
    return func
end
export my_lin_interp
end