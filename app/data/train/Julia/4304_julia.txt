using BinDeps

@BinDeps.setup

const libxbraid = library_dependency("libbraid", aliases=["libbraid", "libxbraid"])
const xbraidver = "2.1.0"
const xbraidfilebase = "braid_$(xbraidver)"
const prefix = BinDeps.usrdir(libxbraid)
const srcdir = joinpath(BinDeps.srcdir(libxbraid), "braid")
rm(srcdir, recursive=true, force=true)

## Add support for basic "make flags"
if "clean" in ARGS || "clean-all" in ARGS
    dirsToRm = [BinDeps.srcdir(libxbraid)]
    "clean-all" in ARGS && push!(dirsToRm, prefix, BinDeps.downloadsdir(libxbraid))
    for dir in dirsToRm
        rm(dir, recursive = true, force = true)
    end
    exit(0)
end

provides(Sources,
         URI("https://computation.llnl.gov/projects/parallel-time-integration-multigrid/download/$(xbraidfilebase).tar.gz"),
         libxbraid,
         unpacked_dir = "braid")

println("Installing XBraid source to ", srcdir)


#TODO: Make build work cross-platform (at least on Unices)
const soname = ifelse(is_apple(), "dylib", "so")
const postfixfile = joinpath(dirname(@__FILE__), ifelse(is_apple(), "makefile.postfix-osx", "makefile.postfix-linux"))
const userMakefile = joinpath(dirname(@__FILE__), "makefile.user")
const srcMakefile = joinpath(srcdir, "Makefile")
if !isfile(srcMakefile) || (readchomp(`tail -n 3 $(srcMakefile)`) != readchomp(`tail -n 3 $(postfixfile)`))
    const appendcmd = pipeline(`cat $(postfixfile)`, stdout = srcMakefile, append = true)
else
    const appendcmd = `echo "$(postfixfile) already appended"`
end
const mpicc = get(ENV, "JULIA_MPI_C_COMPILER", chomp(readstring(`which mpicc`)))

provides(SimpleBuild,
(@build_steps begin
    GetSources(libxbraid)
    @build_steps begin
        ChangeDirectory(srcdir)
        `cp $(userMakefile) $(srcdir)/makefile.user`
        pipeline(`echo "MPICC=$(mpicc)"`, stdout=joinpath(srcdir, "makefile.user"), append=true)
        pipeline(`echo "SONAME=$(soname)"`, stdout=joinpath(srcdir, "makefile.user"), append=true)
        appendcmd
        `make libbraid.$(soname)`
        `make install prefix=$(prefix)`
    end
end),
libxbraid,
os=:Unix)

@BinDeps.install Dict(:libbraid => :libxbraid)
