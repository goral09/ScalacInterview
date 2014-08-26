package scalac.interview

import Figures._
import Solver._

object App extends App {

	val (questionFigures, questionBoard) = (List(King(), King(), Bishop(), Bishop(), Queen(), Queen(), Knight()), Board()(Size(7,7)))
	val (ex1Figures, ex1Board) = (List(King(), King(), Rook()), Board()(Size(3,3)))

	

	val solution = solve(ex1Figures, ex1Board)
	println(solution)
}