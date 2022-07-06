using cme257demo
using Base.Test

@test exprand() >= 0

@test demofun(1, 2) == 3

@test_throws MethodError demofun(3.0, 3.0)
