object Test {
	println("poczatek")                       //> poczatek
	
	val figures: List[Figure] = List(King(), King(), Rook())
                                                  //> figures  : List[Figure] = List(K, K, R)
	val board = Board(List.empty)(Size(3,3))  //> board  : Board = Board@6ae04d
  
  var solutions: Int = 0                          //> solutions  : Int = 0
	
	def solve(figuresLeft: List[Figure], board: Board): List[List[(Figure, (Int, Int))]] = { ??? }
                                                  //> solve: (figuresLeft: List[Figure], board: Board)List[List[(Figure, (Int, Int
                                                  //| ))]]
	solve(figures, board)                     //> scala.NotImplementedError: an implementation is missing
                                                  //| 	at scala.Predef$.$qmark$qmark$qmark(Predef.scala:225)
                                                  //| 	at Test$$anonfun$main$1.solve$1(Test.scala:9)
                                                  //| 	at Test$$anonfun$main$1.apply$mcV$sp(Test.scala:10)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at Test$.main(Test.scala:1)
                                                  //| 	at Test.main(Test.scala)
	
	solutions
                               
 println("koniec")
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
	//TODO got a feeling these two functions can be combined into one
	val isAnyGuarding: List[(Figure, (Int, Int))] => Tuple2[Int, Int] => Boolean =
		list => pos => list.forall { case (figure, place) => isGuarding(figure, place)(pos) }
		
	val isAttackingAny: (Figure, (Int, Int)) => List[(Figure, (Int, Int))] => Boolean =
		(figure, place) => figures => figures.exists({case (_, pl) => isGuarding(figure, place)(pl) })
	private def isGuarding(figure: Figure, pl: (Int, Int))(place: (Int, Int)): Boolean = figure guards(pl, place)
}

case class Size(x: Int, y: Int)

class Board(val figures: List[(Figure, (Int, Int))])(implicit boardSize: Size) extends TilesGuarded {
		def placeFigure(f: Figure, p: (Int, Int)) = Board((f, p) ::  figures)
		//Returns first empty and unguarded tile on board
		def emptyTiles = (1 to boardSize.x).toList.flatMap(col => (1 to boardSize.y).toList.map(row => (col, row))).view.filter({
				case (x,y) => !isAnyGuarding(figures)((x,y))
		})
		def getFirstEmpty(figure: Figure) = emptyTiles.collectFirst({case field if!isAttackingAny(figure, field)(figures) => field })
}
	
object Board {
	def apply(Board: List[(Figure, (Int, Int))])(implicit size: Size): Board =
		new Board(Board.sortBy[(Int, Int)](_._2))
}