package scalac.interview

import Figures._

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
    (figure, place) => figures => figures.exists {case (_, pl) => isGuarding(figure, place)(pl) }
    
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
    def emptyTiles(figure: Figure) = {
      val lastFg = figures.reverse.find({case (f,p) => figure == f})
      val (startPosX, startPosY) = lastFg match {
        case Some((fig, posi)) => (posi._1, posi._2)
        case None => (1,1)
      }
      
      (startPosX to boardSize.x).toList.flatMap(col => (startPosY to boardSize.y).toList.map(row => (col, row))).view.filter({
        case (x, y) => (!isAnyGuarding(figures)((x, y)) && !isAttackingAny(figure, (x, y))(figures))
      })
    }
}
object Board {
  def apply(Board: List[(Figure, (Int, Int))] = List.empty)(implicit size: Size): Board =
    new Board(Board.sortBy[(Int, Int)](_._2))
}