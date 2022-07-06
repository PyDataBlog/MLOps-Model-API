#=
tic()
fh = open("gene_all")
gene_list = Set{ASCIIString}()
for line in readlines(fh)
    push!(gene_list, line[1:end-1])
end
toc()
=#
info("gene list done")
fh2 = open("variant_summary.txt")
readlines(fh2)
#=
for line in readlines(fh2)
    #if startswith(line, "#")
    #    continue
    #else
    gene_symbol = split(line, "\t")[5]
    gene_symbol = convert(ASCIIString,gene_symbol)
    if in(gene_symbol,gene_list)
        print(line)
    end
  #  end
end
=#
