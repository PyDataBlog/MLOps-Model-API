var ratio = require('ratio')

function error(actual, expected) {
    return Math.abs(actual - expected) / expected
}

function approx(target, max) {

    max = (max || 10)
    // find a good approximation

    var best = 1, j, e, result

    for (var i = 1; i < max; i++) {
        j = Math.round(i * target)

        e = error(j / i, target)
        if (e >= best) continue

        best = e
        result = ratio(j, i)
    }

    return result
}

module.exports = approx
