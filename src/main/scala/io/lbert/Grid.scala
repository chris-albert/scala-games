package io.lbert

case class Grid(height: Int, width: Int) {

  def map[A](f: Coord => A): Seq[A] =
    (0 until height).flatMap { row =>
      (0 until width).map { col =>
        f(Coord(col, row))
      }
    }

  def filter(p: Coord => Boolean): Seq[Coord] =
    map(c => if (p(c)) Some(c) else None).flatten

  def size: Int = height * width

  def offset(offset: Int): Option[Coord] = {
    if(offset > (height * width) - 1) None
    else if(offset < 0) None
    else {
      val x = offset % width
      val y = Math.floor(offset / width).toInt
      Some(Coord(x, y))
    }
  }
}

object Grid {
  def coord(grid: Grid): Seq[Coord] = grid.map(identity)

  def isBounded(grid: Grid, coord: Coord): Boolean =
    coord.x >= 0 && coord.x < grid.width && coord.y >= 0 && coord.y < grid.height

  def getMiddleCoord(grid: Grid): Coord = Coord((grid.width / 2) - 1, (grid.height / 2) - 1)
}
