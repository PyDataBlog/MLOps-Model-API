package com.github.PhysicsEngine.scalanlptutorial
import breeze.linalg._
import breeze.numerics._

object App {
  def main(args: Array[String]) {
	  // 4*4 matrix
	  val T = DenseMatrix((.0, .0, .0, .0), (1.0, .0, .5, .0), (.0, 1.0, .0, .0), (.0, .0, .5, .0))

	  val d = 0.85

	  // Row number of T matrix
	  val N = T.rows
	  // Transposed matrix of [1/N, 1/N, 1/N, 1/N, 1/N] 
	  var r = DenseMatrix(Array.fill(N)(1.0/N)).t
	  
	  val u = r.copy
	  
	  val m = 20
	  
	  for(i <- 1 to m){
		  r = d*T*r + (1-d)*u
	  }	
	  
	  print(r)
  }

}
