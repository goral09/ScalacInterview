package scalac.interview


case class Position(x: Int, y: Int) {
  override def toString: String = s"($x,$y)"
  def nextInSize(s: Size): Position = (x,y) match {
    case (x,y) if x == s.x && y == s.y => Position(x, y)
    case (x,y) if x == s.x => Position(x, y + 1)
    case (x,y) if y == s.y => Position(x + 1, 1)
    case (x,y) => Position(x + 1, y)
  }
}

object Position {
  implicit def tplToPos(a: (Int, Int)) = Position(a._1, a._2)
  implicit def positionOrdering: Ordering[Position] = new Ordering[Position] {
    def compare(a: Position, b: Position) = (a.x + a.y) - (b.x  + b.y)
  }
}

trait Monoid[T] {
  def unit: T
  def append(a: T, b: T): T
}

object PositionMonoid {
  implicit val posMonoid: Monoid[Position] = new Monoid[Position] {
    def unit: Position = Position(0, 0)
    def append(p1: Position, p2: Position): Position = Position(p1.x + p2.x, p1.y + p2.y)
  }
}

trait MonoidSyntax[T] {
  def |+|(x: T): T
}

object MonoidSyntax {
  implicit def positionMonoidSyntax(p1: Position)(implicit mt: Monoid[Position]): MonoidSyntax[Position] = new MonoidSyntax[Position] {
    def |+|(p2: Position): Position = mt.append(p1, p2)
  }
}

