local goo = tostring(_ENV)

all = {}

for k, v in pairs(_G) do
    all["_G " .. k] = v
end

for k, v in pairs(_ENV) do
    all["_ENV " .. k] = v
end

for k, v in pairs(all) do
    print(k .. " = " .. tostring(v))
end
