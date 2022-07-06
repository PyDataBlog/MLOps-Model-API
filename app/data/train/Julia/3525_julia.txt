module MutID

using Compat
using DataFrames

function preprocess(csv_file, idx_mut)
    out_file= string(csv_file[1:end-4], "_new.tsv")
    out = open(out_file, "w")
    open(csv_file) do file
        while !eof(file)
            line = readline(file)
            #@show line
            row  = convert(Array{(@compat String),1}, split(line, ","))
            new_row = row[1:7], row[]
            #@show row
            mutid = row[idx_mut]
            mutids = convert(Array{(@compat String),1}, split(split(mutid,"\\")[2][4:end], ","))
            num_mutid = length(mutids)
            for i in 1:num_mutid
                out_row = vcat(row, mutids[i])
                @show length(out_row)
                write(out, join(out_row,"\t"), '\n')
            end
        end
    end
    close(out)
end

#preprocess("data/re.csv", 8)
#preprocess("data/stomach.csv", 6)
cosmic_path = "/haplox/data/cosmic/GRch37/v74/CosmicCompleteExport.tsv"
cosmic = readtable(cosmic_path, header=true) #x17
re = readtable("data/re.csv", header=false) # end
sto = readtable("data/stomach.csv", header=false) #end

function preprocess_dataframe(df, idx_mut)
    mutids = map(x->split(x,"\\")[2][4:end], df[idx_mut]) |>s-> map(x->split(x,","), s)
    ret_df = DataFrame()
    rowidx_df = 0
    for mutid in mutids
        rowidx_df += 1
        num_id = length(mutid)
        for i in 1:num_id
            ret_df = vcat(ret_df, df[rowidx_df,:])
        end
    end
    new_mutids = reduce(vcat, mutids) |> x->convert(Array{UTF8String,1}, x)
    
    hcat(ret_df, new_mutids)
end

re = preprocess_dataframe(re, 8)
sto = preprocess_dataframe(sto, 6)
#@show head(re,1)
#@show head(cosmic,1)
rename!(re, :x1_1, :Mutation_ID)
rename!(sto, :x1_1, :Mutation_ID)
ret_re = join(re, cosmic, on=:Mutation_ID, kind=:left)
ret_sto = join(sto, cosmic, on=:Mutation_ID, kind=:left)

writetable("ret_re.csv", ret_re)
writetable("ret_stomach.csv", ret_sto)

end
