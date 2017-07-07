package io.lbert.connectn

import io.lbert.connectn.ConnectN._
import org.scalatest.{Matchers, WordSpec}

class ConnectNSpec extends WordSpec with Matchers {

  "play" should {
    "drop red to bottom left" in {
      val state = GameState(Board(4,4))
      val outState = play(state,0, Red)
      outState shouldBe Some(state.copy(pieces = Seq(Coord(0,0) -> Red)))
    }
    "drop black on top of red" in {
      val state = GameState(Board(4,4))
      val outState = makeMoves(state, Seq(
        0 -> Red,
        0 -> Black
      ))
      outState shouldBe Some(state.copy(pieces = Seq(
        Coord(0,0) -> Red,
        Coord(0,1) -> Black
      )))
    }
    "fail if dropping too many" in {
      val state = GameState(Board(2,2))
      val outState = makeMoves(state, Seq(
        0 -> Red,
        0 -> Black,
        0 -> Red
      ))
      outState shouldBe None
    }
  }

  "getSlices" should {
    "get simple slice" in {
      val board = Board(4,4,4)
      val coord = Coord(0,0)
      getSlices(board, coord) shouldBe Seq(
        Seq(Coord(0,0),Coord(0,1),Coord(0,2),Coord(0,3)),
        Seq(Coord(0,0),Coord(1,0),Coord(2,0),Coord(3,0))
      )
    }
  }

  "isGameOver" should {
    "be over" in {
      val state = GameState(Board(4,4),Seq(
        Coord(0,0) -> Red,
        Coord(0,1) -> Red,
        Coord(0,2) -> Red,
        Coord(0,3) -> Red
      ))
      isGameOver(state) shouldBe true
    }
    "be not over" in {
      val state = GameState(Board(4,4),Seq(
        Coord(0,0) -> Red,
        Coord(0,1) -> Black,
        Coord(0,2) -> Red,
        Coord(0,3) -> Red
      ))
      isGameOver(state) shouldBe false
    }
  }

  private def makeMoves(state: GameState, moves: Seq[(Int,GamePiece)]): Option[GameState] = {
    def loop(s: GameState, m: List[(Int,GamePiece)]): Option[GameState] = m match {
      case Nil => Some(s)
      case x :: xs => play(s,x._1,x._2).flatMap(loop(_,xs))
    }
    loop(state,moves.toList)
  }
}

