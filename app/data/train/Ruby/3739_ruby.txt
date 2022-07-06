def max_rot(n)
  holder = make_sequence(n)
  return make_integer(holder).sort!.max
end


def make_integer(arr)
  arr.map! { |el| el.join("").to_i }
end


def make_sequence(num)
  storage_arr = []
  arr = num.to_s.split("")
  storage_arr << arr
  rotated_arr = arr.rotate
  storage_arr << rotated_arr

  index = 0
  while index < (rotated_arr.length - 2)
    clone_arr = rotated_arr.dup
    beginning = clone_arr.slice!(0..index)
    final_arr = (beginning << clone_arr.rotate).flatten
    rotated_arr = final_arr
    storage_arr << rotated_arr
    index += 1
  end
  storage_arr
end


p max_rot(56789)