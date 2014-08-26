package scalac.interview

import Figures._

object Solver {

	def solve(figuresLeft: List[Figure], board: Board): List[List[(Figure, (Int, Int))]] = {
		var tmpSolutions: List[List[(Figure, (Int, Int))]] = List.empty
		def helper(figuresLeft: List[Figure], board: Board):  Unit = figuresLeft.sorted match {
			case figure :: tail if(figuresLeft.size <= board.emptyTilesLeft) ⇒
				board.emptyTiles(figure).toList.foreach(pos ⇒ helper(tail, board.placeFigure(figure, pos)))
			case Nil ⇒ if(tmpSolutions.contains(board.figures)) tmpSolutions ; else tmpSolutions = board.figures :: tmpSolutions

		}
		
		helper(figuresLeft, board)
		tmpSolutions
	}
	
}
