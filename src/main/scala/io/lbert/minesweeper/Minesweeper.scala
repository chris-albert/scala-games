package io.lbert.minesweeper

object Minesweeper {

  case class Grid(row: Int, col: Int) {
    def map[A](f: Coord => A): Seq[A] =
      (0 until row).flatMap { row =>
        (0 until col).map { col =>
          f(Coord(row, col))
        }
      }.toSeq

    def filter(p: Coord => Boolean): Seq[Coord] =
      map(c => if (p(c)) Some(c) else None).flatten

    def coords: Seq[Coord] = map(identity)
  }

  case class Game(grid: Grid, bombs: Seq[Coord] = Seq(), open: Seq[Coord] = Seq(), isDead: Boolean = false) {

    override def toString: String =
      (0 until grid.row).map { r =>
        (0 until grid.col).map { c =>
          coordState(Coord(r, c))
        }.mkString(" ")
      }.mkString("\n") + "\n\nisDead=" + isDead

    def coordState(coord: Coord): String = {
      if (isOpen(coord)) {
        if (isBomb(coord)) "*" else adjacentBombs(coord).toString
      } else "-"
    }

    def adjacentBombs(coord: Coord): Int =
      coord.getSurrounding(grid).map(c => if (isBomb(c)) 1 else 0).sum

    def isOpen(coord: Coord): Boolean = open.contains(coord)

    def isBomb(coord: Coord): Boolean = bombs.contains(coord)

    def isBombAdjacent(coord: Coord): Boolean = bombs.exists(_.isSurrounding(coord))

    def openCoord(coord: Coord): Game = {

      def loop(coords: List[Coord], checked: Set[Coord], accu: List[Coord]): List[Coord] = {
        coords match {
          case Nil => accu
          case head :: rest =>
            val (bombAdjacent,noBombAdjacent) = head
              .getSurrounding(grid)
              .filterNot(isBomb)
              .filterNot(checked.toSeq.contains)
              .partition(isBombAdjacent)
            loop(noBombAdjacent.toList ::: rest, checked + head, (head :: accu) ::: bombAdjacent.toList)
        }
      }
      val wasBomb = isBomb(coord)
      val out = if(wasBomb) {
        Seq()
      } else {
        loop(List(coord),Set(),List())
      }

      this.copy(open = open.++:(out).+:(coord), isDead = wasBomb)
    }
  }

  case class Coord(row: Int, col: Int) {
    def isSurrounding(coord: Coord): Boolean =
      Math.abs(coord.row - row) <= 1 && Math.abs(coord.col - col) <= 1 && this != coord

    def getSurrounding(grid: Grid): Seq[Coord] = grid.filter(_.isSurrounding(this))
  }

}