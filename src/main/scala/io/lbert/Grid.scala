package io.lbert

case class Grid(row: Int, col: Int) {
  def map[A](f: Coord => A): Seq[A] =
    (0 until row).flatMap { row =>
      (0 until col).map { col =>
        f(Coord(row, col))
      }
    }

  def filter(p: Coord => Boolean): Seq[Coord] =
    map(c => if (p(c)) Some(c) else None).flatten

  def size: Int = row * col
}

object Grid {
  def coord(grid: Grid): Seq[Coord] = grid.map(identity)

  def isBounded(grid: Grid, coord: Coord): Boolean =
    coord.x >= 0 && coord.x < grid.col && coord.y >= 0 && coord.y < grid.row

  def getMiddleCoord(grid: Grid): Coord = Coord((grid.col / 2) - 1, (grid.row / 2) - 1)
}
