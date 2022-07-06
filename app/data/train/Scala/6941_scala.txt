/**
 *
 */
package antsColony

/**
 * @author mayas
 *
 */
class pathFinder(field : Array[Array[Int]], goalI : Int, goalJ : Int) {

	val _field = field
	val _goalI = goalI
	val _goalJ = goalJ

	def findPath(pathFindingInstance : PathFindingMethod) : Array[Int] = {
		return pathFindingInstance.getPath(_field, _goalI, _goalJ)
	}
}