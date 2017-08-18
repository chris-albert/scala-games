package io.lbert.snake

import io.lbert.Coord.{Direction, Up}
import io.lbert.{Coord, Grid}

object Snake {

  case class State(grid: Grid, dir: Direction, snake: Seq[Coord], apples: Seq[Coord]) {

    def nextState: Option[State] = {
      val newSnake = snake.map(c => Direction.apply(c,dir,1))
      if(newSnake.count(c => Grid.isBounded(grid,c)) == snake.length) {
        Some(copy(snake = newSnake))
      } else {
        None
      }
    }
  }

  def newState(grid: Grid = Grid(10,10),length: Int = 2): State = {
    val middle = Grid.getMiddleCoord(grid)
    val snake = middle :: (1 until length).map(i => Direction.down(middle,i)).toList
    State(grid, Up, snake, Seq())
  }
}
