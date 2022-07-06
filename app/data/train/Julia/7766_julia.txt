using XBraid

@static if VERSION >= v"0.7-"
    using Test
else
    using Base.Test
end

using Compat
import Compat.String

function calc_nprocs()
    for i in eachindex(ARGS)
        if chomp(lowercase(ARGS[i])) == "-nprocs"
            return parse(Int, ARGS[i + 1])
        end
    end
    return clamp(Sys.CPU_CORES, 2, 4)
end

const nprocs = calc_nprocs()
@static if VERSION >= v"0.7-"
    const exename = joinpath(Sys.BINDIR, Base.julia_exename())
else
    const exename = joinpath(JULIA_HOME, Base.julia_exename())
end

const testdir = dirname(@__FILE__)
istest(f) = endswith(f, ".jl") && f != "runtests.jl"
const testfiles = sort(filter(istest, readdir(testdir)))

# Following code for running tests using MPI borrowed from MPI.jl runtests file

# Code coverage command line options; must correspond to src/julia.h
# and src/ui/repl.c
const JL_LOG_NONE = 0
const JL_LOG_USER = 1
const JL_LOG_ALL = 2
const coverage_opts = Dict( JL_LOG_NONE => "none",
                            JL_LOG_USER => "user",
                            JL_LOG_ALL => "all")
const coverage_opt = coverage_opts[Base.JLOptions().code_coverage]

function run_testf(fn::AbstractString)
    testf = joinpath(testdir, fn)
    testcmd = Cmd(
        `mpiexec --verbose -n $nprocs $exename --code-coverage=$coverage_opt $testf`,
        ignorestatus = true
    )
    testfout = joinpath(@__DIR__, string(fn, "-stdout.log"))
    testferr = joinpath(@__DIR__, string(fn, "-stderr.log"))
    try
        info("Running $(testcmd)")
        passed = success(pipeline(testcmd, stdout = testfout, stderr = testferr))
        outStr = chomp(read(testfout, String))
        errStr = chomp(read(testferr, String))
        for (str, nm) in [(outStr, "STDOUT"), (errStr, "STDERR")]
            if !isempty(str)
                println(STDERR, nm, ":\n", str, "\n")
            else
                println(STDERR, nm, ": None\n")
            end
        end
        return passed
    catch ex
        Base.with_output_color(:red,STDERR) do io
            println(io,"\tError: $testf")
            showerror(io, ex, catch_backtrace())
            println(io, "")
        end
        return false
    end
    return false
end

@testset "XBraid" begin
@testset "Test file $f" for f in testfiles
    @test run_testf(f)
end
end