"""
Test building a grid of lipids.

Arguments
----------
input_file:AbstractString
    path to the PDB file containing a lipid molecule

output_file:AbstractString
    path to the output PDB file 

pdb_style:DataType
    PDB format style 

grid_style:DataType
    style of the grid

directions:AbstractArray 
    directions array 

spacings:Tuple
    spacings between two neighboring grid points

counts:Tuple
    number of grid points

msg="":AbstractString
    (optional) test message 

clash_cutoff=1e-3:AbstractFloat
    (keyword) cutoff distance for clashing
"""
function test_build_random_lipid_grid(input_file::AbstractString, 
    output_file::AbstractString, pdb_style::DataType,
    grid_style::DataType,
    directions::AbstractArray, 
    spacings::Tuple, 
    counts::Tuple,
    msg::AbstractString="",
    clash_cutoff::AbstractFloat=1e-3)

    print_dashed_line(80)
    print_with_color(:blue, "Test build randomized lipid grids\n\n")
    if msg != ""
        print_with_color(:blue, "$msg\n\n")
    end
    @time system = obtain(MolecularPDB.read(pdb_style, input_file), :molecular_system)
    @time new_system = fuse(MolecularSystem, build(grid_style, system; directions=directions, spacings=spacings, counts=counts, clash_cutoff=clash_cutoff))
    @time save(pdb_style, output_file, PDB(molecular_system=new_system))
    
    print_with_color(:green, "VERIFIED! build lipid grid\n\n")
    print_dashed_line(80)
    return true
end

# include("unit_test_1_build_random_lipid_grid.jl")
include("unit_test_2_build_random_lipid_grid.jl")
