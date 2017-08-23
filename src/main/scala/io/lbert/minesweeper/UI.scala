package io.lbert.minesweeper

import com.googlecode.lanterna.graphics.{BasicTextImage, TextImage}
import com.googlecode.lanterna.input.KeyType
import io.lbert.LaternaUI._
import io.lbert.{Coord, Grid, LaternaUI}
import io.lbert.minesweeper.Minesweeper.Game

object UI extends App with LaternaUI {

  case class Chars(bomb: TextChar = TextChar('B'),
                   open: TextChar = TextChar(' '),
                   closed: TextChar = TextChar(0x2588.toChar),
                   border: TextChar = TextChar('X'))

  val chars = Chars()

  def getGridImage(game: Game): TextImage = {
    val image = new BasicTextImage(Size(game.grid.width,game.grid.height))
    val bombHash = game.bombs.toSet
    val openHash = game.open.toSet
    (0 until game.grid.height).map(row =>
      (0 until game.grid.width).map{ col =>
        val coord = Coord(col, row)
        if(bombHash.contains(coord) && openHash.contains(coord)) {
          image.setCharacterAt(Pos(col,row),chars.bomb)
        } else if(openHash.contains(coord)) {
          val sur = Coord.getSurrounding(Coord(col,row),game.grid)
          val bombSet = game.bombs.toSet
          val c = sur.count(c => bombSet.contains(c))
          if(c == 0) {
            image.setCharacterAt(Pos(col, row), chars.open)
          } else {
            image.setCharacterAt(Pos(col, row), TextChar((c + 48).toChar))
          }
        } else {
          image.setCharacterAt(Pos(col,row),chars.closed)
        }
      }
    )
    image
  }

  def drawGame(game: Game) = {
    textGraphics.drawImage(Pos(3,2),getGridImage(game))
    screen.refresh()
  }

  def readInput(game: Game, bounds: Rectangle): Unit = {
    val currentPos = Pos(screen.getCursorPosition)
    val stroke = screen.readInput()
    stroke.getKeyType match {
      case KeyType.Character if stroke.getCharacter == 'q' =>
      case KeyType.Enter =>
        val newGame = game.openCoord(Pos.offset(currentPos,bounds.pos).toCoord)
        drawGame(newGame)
        screen.readInput()
        if(!newGame.isDead) {
          readInput(newGame,bounds)
        } 
      case any =>
        readBoundedInput(bounds,any,currentPos).map{newPos =>
          screen.setCursorPosition(newPos)
          screen.refresh()
        }
        readInput(game,bounds)
    }
  }

  run {
    val game = Game.randomBombs(Game(Grid(24, 24)),100)
    val offset = Pos(3,2)
    screen.setCursorPosition(offset)
    textGraphics.drawRectangle(Pos(0,0),Size(game.grid.width + 6, game.grid.height + 4),chars.border)
    drawGame(game)
    readInput(game,Rectangle(offset,Size(game.grid.width,game.grid.height)))
  }


}
