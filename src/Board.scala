package scalac.interview

import Figures._

case class Size(x: Int, y: Int)

class Board(val figures: List[(Figure, (Int, Int))])(implicit boardSize: Size) extends TilesGuarded {
    //tracing helper fction
    def trace[A](msg: String, a: A): A = {
      println(s"$msg $a")
      a
    }
    
    def placeFigure(f: Figure, p: (Int, Int)) = Board((f, p) ::  figures)
    //Returns first empty and unguarded tile on board
    def emptyTiles(figure: Figure) = (1 to boardSize.x).toList.flatMap(col => (1 to boardSize.y).toList.map(row => (col, row))).view.filter({
        case (x,y) => !(isAnyGuarding(figures)((x,y)) && isAttackingAny(figure, (x,y))(figures))
    })
    
}
  
object Board {
  def apply(Board: List[(Figure, (Int, Int))])(implicit size: Size): Board =
    new Board(Board.sortBy[(Int, Int)](_._2))
}

trait TilesGuarded {
  val isAnyGuarding: List[(Figure, (Int, Int))] => Tuple2[Int, Int] => Boolean =
    list => pos => list.forall { case (figure, place) => isGuarding(figure, place)(pos) }
    
  val isAttackingAny: (Figure, (Int, Int)) => List[(Figure, (Int, Int))] => Boolean =
    (figure, place) => figures => figures.exists({case (_, pl) => isGuarding(figure, place)(pl) })
    
  private def isGuarding(figure: Figure, pl: (Int, Int))(place: (Int, Int)): Boolean = figure guards(pl, place)
}