package io.lbert.connectn

import io.lbert.Coord
import io.lbert.connectn.ConnectN._
import org.scalatest.{Matchers, WordSpec}
import io.lbert.connectn.ConnectNASCII._

class ConnectNASCIISpec extends WordSpec with Matchers{

  "draw" should {
    "draw blank board" in {
      render(GameState(Board(4,4)),ASCIIChars()) shouldBe Seq(
        "| X | X | X | X |",
        "| X | X | X | X |",
        "| X | X | X | X |",
        "| X | X | X | X |"
      )
    }
    "draw board with left col full" in {
      val state = GameState(Board(4,4), Seq(
        Coord(0,0) -> Red,
        Coord(0,1) -> Black,
        Coord(0,2) -> Red,
        Coord(0,3) -> Black
      ))
      render(state,ASCIIChars()) shouldBe Seq(
        "| B | X | X | X |",
        "| R | X | X | X |",
        "| B | X | X | X |",
        "| R | X | X | X |"
      )
    }
  }
}
