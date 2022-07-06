type PassiveAggresive
    numClass::Int
    numFeature::Int
    weight::Array
    function PassiveAggresive(numClass, numFeature)
        weight = rand(numClass, numFeature)
        return new(numClass, numFeature, weight)
    end
end

function train(passiveAggresive::PassiveAggresive, features::Array, label::Int)
    losses = passiveAggresive.weight * features
    r = label
    s = -1
    for (index, loss) in enumerate(losses)
        if index == label continue end
        if s == -1 || loss > losses[s] s = index end
    end
    loss = max(0.0, 1.0 - (losses[r] - losses[s]))
    if loss > 0
        z = zeros(passiveAggresive.numClass)
        z[r] = 1
        z[s] = -1
        z = z*features'
        n = sum(map(a -> a*a, z))
        #tau = loss/n
        tau = loss/(n + 1.0/4.0) # PA-2
        passiveAggresive.weight += tau*z
    end
end

function predict(passiveAggresive::PassiveAggresive, features::Array)
    losses = passiveAggresive.weight * features
    return indmax(losses)
end

