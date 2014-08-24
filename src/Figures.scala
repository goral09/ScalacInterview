package scalac.interview

object Figures {
	sealed trait Figure {
		def guards(pos1: (Int, Int), pos2: (Int, Int)): Boolean
	}
	case class Queen()  extends Figure {
		override def toString = "Q"
		def guards(myPos: (Int, Int), other: (Int, Int)): Boolean = myPos._1 == other._1 || myPos._2 == other._2 ||
			math.abs(myPos._1 + myPos._2) == math.abs(other._1 + other._2)
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
		def guards(my: (Int, Int), other: (Int, Int)): Boolean = my._1 != other._1 && my._2 != other._2 &&
			( math.abs(my._1 - other._1) + math.abs(my._2 - other._2) == 3)
	}
	case class Rook()   extends Figure {
		override def toString = "R"
		def guards(my: (Int, Int), other: (Int, Int)): Boolean = my._1 == other._1 || my._2 == other._2
	}
}