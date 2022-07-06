# 33 interview cake

def appears_twice(num_arr)
  arr_sum = num_arr.inject(0, :+)
  n = num_arr.length - 1
  sum = (1..n).reduce(:+)
  arr_sum - sum
end
