package io.lbert.util

import io.lbert.{Coord, Grid}
import org.scalatest.{Matchers, WordSpec}

object StringToCoords {
  def apply(str: String,
            openChar: Char = '_',
            closedChar: Char = 'X'): Seq[Coord] = {
    val stripped = str.substring(
      Math.min(str.indexOf(openChar), str.indexOf(closedChar)),
      Math.max(str.lastIndexOf(openChar), str.lastIndexOf(closedChar)) + 1
    )
    val split = stripped.split("\n")
    val height = split.length
    val width = split.headOption.map(_.length).getOrElse(0)
    val grid = Grid(height, width)
    stripped.replaceAll("\\s","").zipWithIndex.flatMap{ case (char,index) =>
      if(char == closedChar) grid.offset(index)
      else None
    }
  }
}

class StringToCoords extends WordSpec with Matchers {

  "apply" should {
    "convert string to seq of coords" in {
      StringToCoords(
        """
          |_______
          |_______
          |___X___
          |___X___
          |_______
          |_______
        """.stripMargin
      ) shouldBe Seq(
        Coord(3,2), Coord(3,3)
      )
    }
    "make sure we get first and last" in {
      StringToCoords(
        """
          |X______
          |_______
          |_______
          |_______
          |_______
          |______X
        """.stripMargin
      ) shouldBe Seq(
        Coord(0,0), Coord(6,5)
      )
    }
  }
}
