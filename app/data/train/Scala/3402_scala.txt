
object Board {

    class Board(rows: Int = 50, columns: Int = 50) {
        var board: Array[Array[Int]] = Array.ofDim[Int](rows, columns)

        def updatePosition(player_id: Int, position: List[Int]): Array[Array[Int]] = {
            if(board contains player_id){
                println("caught")
                return board
            }

            board(position.head)(position(1)) = player_id
            board
        }

        def prnt() = {
            println(this.board.map(_.mkString).mkString("\n"))
        }
    }
}
