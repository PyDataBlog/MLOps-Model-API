"""
Return a the path to the requested directory.

Arguments
----------
dir:Symbol
    target directory name 
"""
function test_dir(dir::Symbol) 
    if dir == :data
        return joinpath(pwd(),"..","data")
    elseif dir == :tmp
        return joinpath(pwd(),"..","tmp")
    else
        return ""
    end
end