package io.lbert.snake

import com.googlecode.lanterna.input.KeyType
import io.lbert.Coord.{Down, Left, Right, Up}
import io.lbert.{Grid, LaternaUI}
import io.lbert.LaternaUI.{Pos, TextChar, TextImage}
import io.lbert.snake.Snake.State

object SnakeUI extends App with LaternaUI {

  case class Chars(snake: TextChar = TextChar('X'),
                   apple: TextChar = TextChar('A'),
                   border: TextChar = TextChar('X'),
                   open: TextChar = TextChar('_'))

  val chars = Chars()

  def getImage(state: State): TextImage = {
    val image = TextImage(state.grid)
    image.setAll(chars.open)
    state.snake.map(coord =>
      image.setCharacterAt(Pos(coord),chars.snake)
    )
    image
  }

  def drawGame(state: State) = {
    textGraphics.drawImage(Pos(0,0), getImage(state))
    screen.refresh()
  }

  def readInput(state: State): Unit = {
    val stroke = screen.readInput()
    (stroke.getKeyType match {
      case KeyType.ArrowUp    => Some(Up)
      case KeyType.ArrowRight => Some(Right)
      case KeyType.ArrowDown  => Some(Down)
      case KeyType.ArrowLeft  => Some(Left)
      case _ => None
    }).map { direction =>
      state.copy(dir = direction).move match {
        case Some(newState) =>
          drawGame(newState)
          readInput(newState)
        case None => readInput(state)
      }
    }
  }

  run {
    val game = Snake.newState(Grid(50,50),10)
    drawGame(game)
    readInput(game)
  }
}