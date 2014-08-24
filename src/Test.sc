object Test {
	
	implicit val figuresOrdering = new Ordering[Figure] {
		def compare(f1: Figure, f2: Figure): Int = f1.value compare f2.value
	}                                         //> figuresOrdering  : Ordering[Figure] = Test$$anonfun$main$1$$anon$1@181c5f3
	
	val figures: List[Figure] = List(Rook(), Rook(), Knight(),Knight(), Knight(), Knight())
                                                  //> figures  : List[Figure] = List(R, R, N, N, N, N)
	
	val board = Board(List.empty)(Size(4,4))  //> board  : Board = Board@43840
	

	
	def solve(figuresLeft: List[Figure], board: Board): List[List[(Figure, (Int, Int))]] = figuresLeft.sorted match {
		case figure :: tail if(figuresLeft.size <= board.emptyTilesLeft) =>
			board.emptyTiles(figure).toList.flatMap(pos => solve(tail, board.placeFigure(figure, pos)))
		case Nil => board.figures :: Nil
	}                                         //> solve: (figuresLeft: List[Figure], board: Board)List[List[(Figure, (Int, Int
                                                  //| ))]]
	solve(figures, board).distinct.size       //> res0: Int = 8
	
}

sealed trait Figure {
	val value: Int
	def guards(pos1: (Int, Int), pos2: (Int, Int)): Boolean
}
case class Queen()  extends Figure {
	val value = 1
	override def toString = "Q"
	def guards(my: (Int, Int), other: (Int, Int)): Boolean = my._1 == other._1 || my._2 == other._2 ||
		math.abs(my._1 - my._2) == math.abs(other._1 - other._2)
}
case class Bishop() extends Figure {
	val value = 2
	override def toString = "B"
	def guards(my: (Int, Int), other: (Int, Int)): Boolean = math.abs(my._1 - my._2) == math.abs(other._1 - other._2)
}
case class King()   extends Figure {
	val value = 4
	override def toString = "K"
	def guards(my: (Int, Int), other: (Int, Int)): Boolean = math.abs(my._1 - other._1) <= 1 && math.abs(my._2 - other._2) <= 1
}
case class Knight() extends Figure {
	val value = 3
	override def toString = "N"
	def guards(my: (Int, Int), other: (Int, Int)): Boolean = my._1 != other._1 && my._2 != other._2 &&
		( math.abs(my._1 - other._1) + math.abs(my._2 - other._2) == 3)
}
case class Rook()   extends Figure {
	val value = 2
	override def toString = "R"
	def guards(my: (Int, Int), other: (Int, Int)): Boolean = my._1 == other._1 || my._2 == other._2
}

trait TilesGuarded {
	/*
		Checks if any of already placed <b>figures</b> is already guarding passed <b>pos</b>
	*/
	val isAnyGuarding: List[(Figure, (Int, Int))] => Tuple2[Int, Int] => Boolean =
		figures => pos => figures.exists { case (figure, place) => isGuarding(figure, place)(pos) }
	/*
		Checks if placing <b>figure</b> on <b>place</b> attacks any of <b>figures</b>
	*/
	val isAttackingAny: (Figure, (Int, Int)) => List[(Figure, (Int, Int))] => Boolean =
		(figure, place) => figures => figures.exists({case (_, pl) => isGuarding(figure, place)(pl) })
		
	private def isGuarding(figure: Figure, pl: (Int, Int))(place: (Int, Int)): Boolean = ( pl == place ) || figure.guards(pl, place)
}

case class Size(x: Int, y: Int)

class Board(val figures: List[(Figure, (Int, Int))])(implicit boardSize: Size) extends TilesGuarded {
		//tracing helper function
		def trace[A](msg: String, a: A): A = {
			println(s"$msg $a")
			a
		}
		
		def placeFigure(f: Figure, p: (Int, Int)) = Board((f, p) ::  figures)
		
		def emptyTilesLeft: Int = boardSize.x * boardSize.y - figures.size
		
		//Returns first empty and unguarded tile on board
		def emptyTiles(figure: Figure) = (1 to boardSize.x).toList.flatMap(col => (1 to boardSize.y).toList.map(row => (col, row))).view.filter({
				case (x,y) => (!isAnyGuarding(figures)((x,y)) && !isAttackingAny(figure, (x,y))(figures))
		})
		
}
	
object Board {
	def apply(Board: List[(Figure, (Int, Int))] = List.empty)(implicit size: Size): Board =
		new Board(Board.sortBy[(Int, Int)](_._2))
}