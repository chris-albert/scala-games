package io.lbert

import org.scalatest.{Matchers, WordSpec}

class GridSpec extends WordSpec with Matchers {

  "offset" should {
    "give offset for first element" in {
      Grid(4,4).offset(0) shouldBe Some(Coord(0,0))
    }
    "give offset for last element of first row" in {
      Grid(4,4).offset(3) shouldBe Some(Coord(3,0))
    }
    "give offset for fist element in second row" in {
      Grid(4,4).offset(4) shouldBe Some(Coord(0,1))
    }
    "give offset for last element of last row" in {
      Grid(4,4).offset(15) shouldBe Some(Coord(3,3))
    }
    "give none if past grid" in {
      Grid(4,4).offset(16) shouldBe None
    }
    "give none if negative" in {
      Grid(4,4).offset(-1) shouldBe None
    }
  }

  "size" should {
    "get size of grid" in {
      Grid(4,4).size shouldBe 16
    }
  }

  "map" should {
    "iterate over all elements starting at Coord(0,0)" in {
      Grid(2,2).map(identity) shouldBe Seq(
        Coord(0,0),
        Coord(1,0),
        Coord(0,1),
        Coord(1,1)
      )
    }
  }
}
