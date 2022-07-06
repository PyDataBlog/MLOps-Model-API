module SDatasets

export data_ball

function say(sth)
  println(sth)
end

function data_ball(N=1, center=[0 0], radius=[1];
          class=NaN, distribution="uniform")
  dim = size(center, 2)
  n_center = size(center, 1)
  n_radius = size(radius, 1)

  width = isnan(class) ? dim : dim + 1
  data = Array(Float64, N, width)
  for i = 1:N
    pt = Array(Float64, 1, dim)
    r = radius[(i - 1) % n_radius + 1]
    while true
      pt[:] = (rand(dim) - .5) * (r * 2)
      if (pt*pt')[1] <= r * r
        break
      end
    end

    data[i,1:dim] = pt + center[(i - 1) % n_radius + 1, :]
    if !isnan(class)
      data[i, width] = class
    end
  end

  data
end

end # module
