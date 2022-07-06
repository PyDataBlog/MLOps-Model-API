"""
Test building a spherical shaped lipid aggregate.

Arguments
----------
input_file:AbstractString
    path to the PDB file containing a lipid molecule

output_file:AbstractString
    path to the output PDB file 

pdb_style:DataType
    PDB format style 

sphere_style:DataType
    style of the sphere

count:Integer
    number of sphere points

radius::AbstractFloat
    radius of the sphere

msg="":AbstractString
    optional test message 

center=[]:AbstractArray 
    (keyword) center of the sphere

aligned:Bool
    (keyword) if ``true``, then the principal axis of each object will
    be aligned to the radial direction.

inverted:Bool
    (keyword) if true, then the final orientation will be inverted
"""
function test_build_lipid_sphere(input_file::AbstractString, 
    output_file::AbstractString, pdb_style::DataType,
    sphere_style::DataType,
    radius::AbstractFloat, count::Integer,
    msg::AbstractString="";
    center::AbstractArray=[], 
    aligned::Bool=true,
    inverted::Bool=false)

    print_dashed_line(80)
    print_with_color(:blue, "Test build lipid spheres\n\n")
    if msg != ""
        print_with_color(:blue, "$msg\n\n")
    end
    system = obtain(MolecularPDB.read(pdb_style, input_file), :molecular_system)
    @time new_system = fuse(MolecularSystem, build(sphere_style, system, count, radius; center=center, aligned=aligned, inverted=inverted))
    @time save(pdb_style, output_file, PDB(molecular_system=new_system))
    
    print_with_color(:green, "VERIFIED! build lipid sphere\n\n")
    print_dashed_line(80)
    return true
end

include("unit_test_1_build_FibonacciSphere.jl")
# include("unit_test_2_build_FibonacciSphere.jl")
