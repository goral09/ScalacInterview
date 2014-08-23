object Test {
	println("poczatek")                       //> poczatek
	
	val figures: List[Figure] = List(Queen(), Queen(), King(), King(), Bishop(), Bishop(), Knight())
                                                  //> figures  : List[Figure] = List(Q, Q, K, K, B, B, N)
	val board = Board(List.empty)(Size(8,8))  //> board  : Board = Board@15f7ae5
  board.figures                                   //> res0: List[(Figure, (Int, Int))] = List()
	
	def solve(figuresLeft: List[Figure], board: Board): List[List[(Figure, (Int, Int))]] = {
		List.empty
	}                                         //> solve: (figuresLeft: List[Figure], board: Board)List[List[(Figure, (Int, Int
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
	val isAnyGuarding: List[(Figure, (Int, Int))] => Tuple2[Int, Int] => Boolean =
		list => pos => list.forall { case (figure, place) => isGuarding(figure, place)(pos) }
	private def isGuarding(figure: Figure, pl: (Int, Int))(place: (Int, Int)): Boolean = figure guards(pl, place)
}

case class Size(x: Int, y: Int)

class Board(val figures: List[(Figure, (Int, Int))])(implicit boardSize: Size) extends TilesGuarded {
		def placeFigure(f: Figure, p: (Int, Int)) = Board((f, p) ::  figures)
		
		//Returns first empty and unguarded tile on board
		def emptyTiles = (1 to boardSize.x).toList.flatMap(col => (1 to boardSize.y).toList.map(row => (col, row))).view.filter({
				case (x,y) => !isAnyGuarding(figures)((x,y))
		})
}
	
object Board {
	def apply(Board: List[(Figure, (Int, Int))])(implicit size: Size): Board =
		new Board(Board.sortBy[(Int, Int)](_._2))
}