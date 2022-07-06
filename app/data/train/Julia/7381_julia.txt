x = 0
for i = 0:999
    for j = 0:999
        for k = 0:99
            x += i * j * k
        end
    end
end
print(x)
