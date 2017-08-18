package io.lbert

case class Coord(x: Int, y: Int)

object Coord {

  def apply(c: Coord)(x: Int = 0, y: Int = 0): Coord =
    c.copy(x = c.x + x, y = c.y + y)

  sealed trait Direction
  case object Up extends Direction
  case object Right extends Direction
  case object Down extends Direction
  case object Left extends Direction

  object Direction {
    def apply(c: Coord, d: Direction, i: Int = 1): Coord = d match {
      case Up => up(c, i)
      case Right => right(c, i)
      case Down => down(c, i)
      case Left => left(c, i)
    }

    def right(c: Coord, i: Int = 1): Coord = Coord(c)(x = i)
    def left(c: Coord, i: Int = 1): Coord = Coord(c)(x = -i)
    def up(c: Coord, i: Int = 1): Coord = Coord(c)(y = -i)
    def down(c: Coord, i: Int = 1): Coord = Coord(c)(y = i)
  }

  def surrounding(c: Coord): Seq[Coord] =
    Seq(Up, Right, Down, Down, Left, Left, Up, Up).foldLeft[List[Coord]](List(c)){
      case (accu,dir) => Direction(accu.head,dir) :: accu
    }.reverse.tail

  def isSurrounding(c1: Coord, c2: Coord): Boolean =
    c1 != c2 && Math.abs(c1.x - c2.x) <= 1 && Math.abs(c1.y - c2.y) <= 1

  def getSurrounding(coord: Coord, grid: Grid): Seq[Coord] =
    grid.filter(isSurrounding(_,coord))
}
