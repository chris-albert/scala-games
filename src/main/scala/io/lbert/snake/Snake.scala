package io.lbert.snake

import io.lbert.Coord.{Direction, Up}
import io.lbert.{Coord, Grid}

object Snake {

  case class State(grid: Grid, dir: Direction, snake: Seq[Coord], apples: Seq[Coord]) {

    def move: Option[State] = {
      if(snake.isEmpty) None
      else {
        val headMove = Direction.apply(snake.head, dir, 1)
        if (Grid.isBounded(grid, headMove)) {
          val newSnake =
            if (snake.size == 1) Seq(headMove)
            else headMove +: snake.init
          Some(copy(snake = newSnake))
        } else {
          None
        }
      }
    }
  }

  def newState(grid: Grid = Grid(10,10),length: Int = 2): State = {
    val middle = Grid.getMiddleCoord(grid)
    val snake = middle :: (1 until length).map(i => Direction.down(middle,i)).toList
    State(grid, Up, snake, Seq())
  }
}
