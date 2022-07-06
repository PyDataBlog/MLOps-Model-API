module TeamsHelper

  def to_pythagoras_data(r, ra, g)
    # r :得点
    # ra:失点
    # g :試合数
    return (g.to_f * (r.to_f ** 2 / (r.to_f ** 2 + ra.to_f ** 2))).to_i
  end
end
