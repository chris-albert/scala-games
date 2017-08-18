package io.lbert.snake

import io.lbert.Coord.Up
import io.lbert.snake.Snake.State
import io.lbert.{Coord, Grid}
import org.scalatest.{AsyncWordSpec, Matchers}

class SnakeSpec extends AsyncWordSpec with Matchers{

  "new state" should {
    "create state with correct snake of length 2" in {
      val state = Snake.newState(Grid(10,10),2)
      state.snake shouldBe Seq(Coord(4,4),Coord(4,3))
    }
    "create state with correct snake of length 4" in {
      val state = Snake.newState(Grid(10,10),4)
      state.snake shouldBe Seq(Coord(4,4),Coord(4,3),Coord(4,2),Coord(4,1))
    }
  }

  "next state" should {
    "move snake up by 1" in {
      val startState = Snake.newState(Grid(10,10),2)
      val state = startState.nextState
      state shouldBe Some(startState.copy(snake = Seq(Coord(4,5),Coord(4,4))))
    }
    "fail to move snake if off grid" in {
      val startState = State(Grid(10,10),Up,Seq(Coord(4,9)),Seq())
      val state = startState.nextState
      state shouldBe None
    }
  }
}
