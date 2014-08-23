object Test {
	println("poczatek")                       //> poczatek
	
	implicit val size = Size(8,8)             //> size  : Size = Size(8,8)
	
	val figures: List[Figure] = List(Queen(), Queen(), King(), King(), Bishop(), Bishop(), Knight())
                                                  //> figures  : List[Figure] = List(Queen(), Queen(), King(), King(), Bishop(), B
                                                  //| ishop(), Knight())
	val board = Board(List((Queen(), (1,1)), (Bishop(), (2,1)), (Knight(), (2,2))))
                                                  //> board  : Board = Board@56dfcb
  board.board                                     //> res0: List[(Figure, (Int, Int))] = List((Queen(),(1,1)), (Bishop(),(2,1)), (
                                                  //| Knight(),(2,2)))
	
	def solve(figuresLeft: List[Figure], Board: Board): List[List[(Figure, (Int, Int))]] = {
		val figure = figuresLeft.head
		List.empty
	}                                         //> solve: (figuresLeft: List[Figure], Board: Board)List[List[(Figure, (Int, Int
                                                  //| ))]]
                               
 println("koniec")                                //> koniec
}

sealed trait Figure
case class Queen()  extends Figure
case class Bishop() extends Figure
case class King()   extends Figure
case class Knight() extends Figure
case class Rook()   extends Figure

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