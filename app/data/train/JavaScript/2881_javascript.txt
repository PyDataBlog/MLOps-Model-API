/**
 * Calculates the radius of the Hill Sphere,
 * for a body with mass `m1`
 * @param  {Number} m1 Mass of the lighter body
 * @param  {Number} m2 Mass of the heavier body
 * @param  {Number} a  Semi-major axis
 * @param  {Number} e  Eccentricity
 * @return {Number}    Hill Sphere radius
 */
function hillSphere( m1, m2, a, e ) {
  return a * ( 1 - e ) * Math.pow(
    ( m1 / ( 3 * m2 ) ), 1/3
  )
}

module.exports = hillSphere
