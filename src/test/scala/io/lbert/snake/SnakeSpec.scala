package io.lbert.snake

import io.lbert.Coord.{Up,Left,Right,Down}
import io.lbert.snake.Snake.State
import io.lbert.util.StringToCoords
import io.lbert.{Coord, Grid}
import org.scalatest.{AsyncWordSpec, Matchers}

class SnakeSpec extends AsyncWordSpec with Matchers{

  "new state" should {
    "create state with correct snake of length 2" in {
      val state = Snake.newState(Grid(10,10),2)
      state.snake shouldBe StringToCoords(
        """
          |__________
          |__________
          |__________
          |__________
          |____X_____
          |____X_____
          |__________
          |__________
          |__________
          |__________
        """.stripMargin
      )
    }
    "create state with correct snake of length 4" in {
      val state = Snake.newState(Grid(10,10),4)
      state.snake shouldBe StringToCoords(
        """
          |__________
          |__________
          |__________
          |__________
          |____X_____
          |____X_____
          |____X_____
          |____X_____
          |__________
          |__________
        """.stripMargin
      )
    }
  }

  "move" should {
    "move snake up by 1" in {
      val startCoords = StringToCoords(
        """
          |__________
          |__________
          |__________
          |__________
          |____X_____
          |____X_____
          |__________
          |__________
          |__________
          |__________
        """.stripMargin
      )
      val startState = State(Grid(10,10), Up, startCoords, Seq())
      val state = startState.move
      val nextCoords = StringToCoords(
        """
          |__________
          |__________
          |__________
          |____X_____
          |____X_____
          |__________
          |__________
          |__________
          |__________
          |__________
        """.stripMargin
      )
      state shouldBe Some(startState.copy(snake = nextCoords))
    }
    "move left up by 1" in {
      val startCoords = StringToCoords(
        """
          |__________
          |__________
          |__________
          |__________
          |____X_____
          |____X_____
          |__________
          |__________
          |__________
          |__________
        """.stripMargin
      )
      val startState = State(Grid(10,10), Left, startCoords, Seq())
      val state = startState.move
      val nextCoords = StringToCoords(
        """
          |__________
          |__________
          |__________
          |__________
          |___XX_____
          |__________
          |__________
          |__________
          |__________
          |__________
        """.stripMargin
      )
      state shouldBe Some(startState.copy(snake = nextCoords))
    }
    "fail to move snake if off grid" in {
      val startState = State(Grid(10,10),Up,Seq(Coord(0,0)),Seq())
      val state = startState.move
      state shouldBe None
    }
  }
}
