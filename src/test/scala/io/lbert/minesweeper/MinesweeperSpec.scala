package io.lbert.minesweeper

import io.lbert.Grid
import io.lbert.minesweeper.Minesweeper.Game
import org.scalatest.{AsyncWordSpec, Matchers}

class MinesweeperSpec extends AsyncWordSpec with Matchers {

  "random bombs" should {
    "get random bombs" in {
      val game = Game(Grid(10,10))
      Game.randomBombs(game,10).bombs.length shouldBe 10
    }
  }
}
