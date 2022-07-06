
function markov_pi(N, delta)
    x, y = 1.0, 1.0
    n_hits = 0
    for i = 1:N
        del_x = (rand() - 0.5) * 0.5 * delta
        del_y = (rand() - 0.5) * 0.5 * delta
        if (abs(x + del_x) < 1.0) && (abs(y + del_y) < 1.0)
            x, y = x + del_x, y + del_y
        end
        if x^2 + y^2 < 1
            n_hits += 1
        end
    end
    n_hits
end

n_trials = 4000000
delta = 0.1

n_hits = markov_pi(n_trials, delta)

println(n_hits)
println(4 * n_hits / n_trials)
