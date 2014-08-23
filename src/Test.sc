object Test {
	println("poczatek")                       //> poczatek
	
	val figures: List[Figure] = List(Queen(), Queen(), King(), King(), Bishop(), Bishop(), Knight())
                                                  //> figures  : List[Figure] = List(Q, Q, K, K, B, B, N)
	val board = Board(List.empty)(Size(8,8))  //> board  : Board = Board@15f7ae5
  board.board                                     //> res0: List[(Figure, (Int, Int))] = List()
	
	def solve(figuresLeft: List[Figure], Board: Board): List[List[(Figure, (Int, Int))]] = {
		val figure = figuresLeft.head
		List.empty
	}                                         //> solve: (figuresLeft: List[Figure], Board: Board)List[List[(Figure, (Int, Int
                                                  //| ))]]
                               
 println("koniec")                                //> koniec
}

sealed trait Figure {
	def guards(pos1: (Int, Int), pos2: (Int, Int)): Boolean
}
case class Queen()  extends Figure {
	override def toString = "Q"
	def guards(myPos: (Int, Int), other: (Int, Int)): Boolean = myPos._1 == other._1 || myPos._2 == other._2 || math.abs(myPos._1 + myPos._2) == math.abs(other._1 + other._2)
}
case class Bishop() extends Figure {
	override def toString = "B"
	def guards(my: (Int, Int), other: (Int, Int)): Boolean = math.abs(my._1 + my._2) == math.abs(other._1 + other._2)
}
case class King()   extends Figure {
	override def toString = "K"
	def guards(my: (Int, Int), other: (Int, Int)): Boolean = math.abs(my._1 - other._1) == 1 || math.abs(my._2 - other._2) == 1
}
case class Knight() extends Figure {
	override def toString = "N"
	def guards(my: (Int, Int), other: (Int, Int)): Boolean = my._1 != other._1 && my._2 != other._2 && ( math.abs(my._1 - other._1) + math.abs(my._2 - other._2) == 3)
}
case class Rook()   extends Figure {
	override def toString = "R"
	def guards(my: (Int, Int), other: (Int, Int)): Boolean = my._1 == other._1 || my._2 == other._2
}

trait TilesGuarded {
	def isGuarding(figure: Figure, pl: (Int, Int))(place: (Int, Int)): Boolean = figure guards(pl, place)
}

case class Size(x: Int, y: Int)

class Board(val board: List[(Figure, (Int, Int))])(implicit boardSize: Size) {
		def placeFigure(f: Figure, p: (Int, Int)) = Board((f, p) ::  board)
		//Returns first empty tile on board
		def firstEmptyTile: Option[(Int, Int)] = {
			val lastFigurePos: (Int, Int) = board.last._2
			val firstEmptyTiletoRet = lastFigurePos match {
				//End of board, no empty tiles left
				case (x,y) if y == boardSize.y && x == boardSize.x => None
				//Last row, but not last column
				case (x,y) if y == boardSize.y => Some((x + 1, 0))
				//Last column, but not last row
				case (x,y) if x == boardSize.x => Some((x, y + 1))
			}
			firstEmptyTiletoRet
		}
}
	
object Board {
	def apply(Board: List[(Figure, (Int, Int))])(implicit size: Size): Board =
		new Board(Board.sortBy[(Int, Int)](_._2))
}