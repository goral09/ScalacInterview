package scalac.interview

import Figures._

object Solver {
	var solNumber: BigInt = 0
	def solve(figuresLeft: List[Figure], board: Board): Unit = {
    var tmpSolutions: List[List[(Figure, (Int, Int))]] = List.empty
    
    def helper(figuresLeft: List[Figure], board: Board): Unit = figuresLeft.sorted match {
      case figure :: tail if (figuresLeft.size <= board.emptyTilesLeft) =>
        board.emptyTiles(figure).toList.foreach({ case (x, y) => helper(tail, board.placeFigure(figure, (x, y))) })
      case Nil => if (tmpSolutions.contains(board.figures)) solNumber = solNumber else tmpSolutions = board.figures :: tmpSolutions; solNumber = solNumber + 1
    }

    helper(figuresLeft, board)
  }
	
}
