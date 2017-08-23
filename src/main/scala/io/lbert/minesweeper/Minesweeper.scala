package io.lbert.minesweeper

import io.lbert.{Coord, Grid}
import scala.util.Random

object Minesweeper {

  case class Game(grid: Grid, bombs: Seq[Coord] = Seq(), open: Seq[Coord] = Seq(), isDead: Boolean = false) {

    def coordState(coord: Coord): String = {
      if (isOpen(coord)) {
        if (isBomb(coord)) "*" else adjacentBombs(coord).toString
      } else "-"
    }

    def adjacentBombs(coord: Coord): Int =
      Coord.getSurrounding(coord,grid).map(c => if (isBomb(c)) 1 else 0).sum

    def isOpen(coord: Coord): Boolean = open.contains(coord)

    def isBomb(coord: Coord): Boolean = bombs.contains(coord)

    def isBombAdjacent(coord: Coord): Boolean = bombs.exists(Coord.isSurrounding(_,coord))

    def openCoord(coord: Coord): Game = {

      def loop(coords: List[Coord], checked: Set[Coord], accu: List[Coord]): List[Coord] = {
        coords match {
          case Nil => accu
          case head :: rest =>
            val (bombAdjacent,noBombAdjacent) = Coord.getSurrounding(head,grid)
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

  object Game {
    def randomBombs(game: Game, amount: Int): Game = {
      def loop(accu: Set[Int]): Set[Int] = {
        if(accu.size >= amount) {
          accu
        } else {
          val r = Random.nextInt(game.grid.size)
          if (accu.contains(r)) {
            loop(accu)
          } else {
            loop(accu + r)
          }
        }
      }
      val bombs = loop(Set()).map{i =>
        val row = Math.floor(i / game.grid.width).toInt
        val col = i - (row * game.grid.width)
        Coord(col,row)
      }
      game.copy(bombs = bombs.toSeq)
    }
  }
}