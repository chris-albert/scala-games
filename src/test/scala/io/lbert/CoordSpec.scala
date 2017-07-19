package io.lbert

import org.scalatest.{Matchers, WordSpec}

class CoordSpec extends WordSpec with Matchers {

  "Direction" should {
    "get right by 1" in {
      Coord.Direction.right(Coord(0,0)) shouldBe Coord(1,0)
    }
    "get right by 5" in {
      Coord.Direction.right(Coord(0,0),5) shouldBe Coord(5,0)
    }
    "get left by 1" in {
      Coord.Direction.left(Coord(0,0)) shouldBe Coord(-1,0)
    }
    "get left by 5" in {
      Coord.Direction.left(Coord(0,0),5) shouldBe Coord(-5,0)
    }
    "get up by 1" in {
      Coord.Direction.up(Coord(0,0)) shouldBe Coord(0,1)
    }
    "get up by 5" in {
      Coord.Direction.up(Coord(0,0),5) shouldBe Coord(0,5)
    }
    "get down by 1" in {
      Coord.Direction.down(Coord(0,0)) shouldBe Coord(0,-1)
    }         
    "get down by 5" in {
      Coord.Direction.down(Coord(0,0),5) shouldBe Coord(0,-5)
    }
  }

  "surrounding" should {
    "get surrounding" in {
      Coord.surrounding(Coord(0,0)) shouldBe Seq(
        Coord(0,1),Coord(1,1),Coord(1,0),Coord(1,-1),Coord(0,-1),Coord(-1,-1),Coord(-1,0),Coord(-1,1)
      )
    }
  }

  "isSurrounding" should {
    "be true for all surrounding coords" in {
      val coord = Coord(0,0)
      Coord.surrounding(coord).map(Coord.isSurrounding(coord,_)).count(b => b) shouldBe 8
    }
    "be false for not surrounding" in {
      Coord.isSurrounding(Coord(0,0),Coord(3,3)) shouldBe false
    }
    "be false if same" in {
      Coord.isSurrounding(Coord(0,0),Coord(0,0)) shouldBe false
    }
  }
}
