export  
    say,              # debug output
    @verbose_say,     # define say according to verbose
    timestr,          # time string, useful for file names
    floatstr,         # short float string
    share,            # make a tuple of array to SharedArray
    load_data,        # load synthetic data
    generate_data     # generate synthetic data

""" debug output """
say = println

""" 
if :verbose not defined, do nothing; 
if verbose true, define say as println; 
else mute say. 
"""
macro verbose_say()
  return esc(quote
    isdefined(:verbose) && (say = verbose ? println : (sth...) -> nothing)
  end) 
end

""" time string, useful for file names, etc. """
timestr() = "$(Dates.format(now(),"yyyymmdd-HHMMSS"))"

""" short float string """
floatstr(num, precision=1000) = "$(floor(num*precision)/precision)"

""" make a tuple of array to SharedArray """
share(t::Tuple) = [convert(SharedArray, i) for i in t]

""" load synthetic data """
function load_data(name="set2")
  file = "data/$name.jld"
  isfile(file) || error("file `$file` not found")
  d = load(file)
  say("dataset `$name` loaded.")
  
  (d["train_data"], d["train_label"], d["validation_data"], d["validation_label"], d["test_data"], d["test_label"])
end

""" generate synthetic data, column vectors """
function generate_data(;name=timestr(), train=25000, validation=5000, test=20000, d=50, p=2, prob=0.9)
  isdir("data") || mkdir("data")
  
  n = train + validation + test
  data = rand([-1, 1], d + 1, n)
  data[end, :] = 1
  
  p_index = sample(1:d, p, replace=false, ordered=true)
  labels = prod(data[p_index, :], 1)[:]
  
  mutate_index = sample(1:n, floor(Int, (1-prob)*n), replace=false, ordered=true)
  mutate = ones(Int, n)
  mutate[mutate_index] = -1
  labels = labels .* mutate
  
  train_index = 1 : train
  validation_index = train + 1 : train + validation
  test_index = train + validation + 1 : n
  
  save("data/$name.jld",  "train_data", data[:, train_index], 
                          "validation_data", data[:, validation_index],
                          "test_data", data[:, test_index],
                          "train_label", labels[train_index],
                          "validation_label", labels[validation_index],
                          "test_label", labels[test_index],
                          "p_index", p_index)
  
  say("data generated in `data/$name.jld`")
end
