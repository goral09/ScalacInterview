package scalac.interview

object Figures {
  
  implicit val figuresOrdering = new Ordering[Figure] {
    def compare(f1: Figure, f2: Figure): Int = f1.value compare f2.value
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
}