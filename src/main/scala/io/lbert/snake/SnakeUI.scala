package io.lbert.snake

import io.lbert.LaternaUI
import io.lbert.LaternaUI.{Pos, TextChar, TextImage}
import io.lbert.snake.Snake.State

object SnakeUI extends App with LaternaUI {

  case class Chars(snake: TextChar = TextChar('X'),
                   apple: TextChar = TextChar('A'),
                   border: TextChar = TextChar('X'))

  val chars = Chars()

  def getImage(state: State): TextImage = {
    val image = TextImage(state.grid)
    image.setAll(TextChar('O'))
    state.snake.map(coord =>
      image.setCharacterAt(Pos(coord),chars.snake)
    )
    image
  }

  def drawGame(state: State) = {
    textGraphics.drawImage(Pos(0,0), getImage(state))
    screen.refresh()
  }

  run {
    val game = Snake.newState()
    drawGame(game)
    screen.readInput()
  }
}