package io.lbert.connectn

import java.nio.charset.Charset
import java.util
import com.googlecode.lanterna.graphics.{BasicTextImage, TextImage}
import com.googlecode.lanterna._
import com.googlecode.lanterna.input.KeyType
import com.googlecode.lanterna.screen.TerminalScreen
import com.googlecode.lanterna.terminal.DefaultTerminalFactory
import io.lbert.Coord
import io.lbert.connectn.ConnectN._

object Laterna extends App {

  val terminal = new DefaultTerminalFactory(System.out, System.in, Charset.forName("UTF8")).createTerminal()
  val screen = new TerminalScreen(terminal)
  val termSize = terminal.getTerminalSize
  val textGraphics = screen.newTextGraphics()

  case class BoardChars(blank: TextChar = TextChar('O'),
                        red: TextChar = TextChar('O', TextColor.ANSI.RED),
                        black: TextChar = TextChar('O', TextColor.ANSI.BLUE),
                        separator: TextChar = TextChar('|'))

  val boardChars = BoardChars()

  case class Pos(x: Int,y: Int) extends TerminalPosition(x,y)
  case class Size(cols: Int, rows: Int) extends TerminalSize(cols,rows)
  case class TextChar(char: Char,
                      foregroundColor: TextColor = TextColor.ANSI.DEFAULT,
                      backgroundColor: TextColor = TextColor.ANSI.DEFAULT)
    extends TextCharacter(char,foregroundColor,backgroundColor,util.EnumSet.noneOf(classOf[SGR]))

  def getBoardImage(gameState: GameState): TextImage = {
    val width = gameState.board.width * 3 + gameState.board.width + 1
    val height = gameState.board.height + 2
    val size = Size(width, height)
    val image = new BasicTextImage(size)
    val piecesMap = gameState.pieces.toMap
    //draw lines
    (0 until gameState.board.height).map(row =>
      (0 until gameState.board.width + 1).map{col =>
        image.setCharacterAt(Pos(col * 4,row),boardChars.separator)
      }
    )
    //draw pieces
    (0 until gameState.board.height).map(row =>
      (0 until gameState.board.width).map{col =>
        val pieceChar = piecesMap.get(Coord(col,row)).map{
          case Red   => boardChars.red
          case Black => boardChars.black
        }.getOrElse(boardChars.blank)
        image.setCharacterAt(Pos(col * 4 + 2,gameState.board.height - row - 1),pieceChar)
      }
    )
    //draw bottom
    (0 until width).map(col => image.setCharacterAt(Pos(col,height - 2),TextChar('-')))
    image.setCharacterAt(Pos(0,height - 1),TextChar('|'))
    image.setCharacterAt(Pos(width - 1,height - 1),TextChar('|'))
    image
  }

  def drawIndicator(col: Int, state: GameState) = {
    val image = new BasicTextImage(Size(state.board.width * 3 + state.board.width + 1,2))
    val piece = nextPiece(state)
    val indicator = piece match {
      case Red   => boardChars.red
      case Black => boardChars.black
    }
    image.setCharacterAt(Pos(col * 4,1),indicator)
    textGraphics.drawImage(Pos(5,1),image)
    screen.refresh()
  }

  def emptyIndicator(state: GameState) = {
    val image = new BasicTextImage(Size(state.board.width * 3 + state.board.width + 1,2))
    textGraphics.drawImage(Pos(5,1),image)
    screen.refresh()
  } 

  def readInput(col: Int, state: GameState): Unit = {
    val key = screen.readInput()
    key.getKeyType match {
      case KeyType.ArrowLeft =>
        if(col - 1 >= 0) {
          drawIndicator(col - 1, state)
          readInput(col - 1, state)
        } else {
          readInput(col, state)
        }
      case KeyType.ArrowRight =>
        if(col + 1 < state.board.width) {
          drawIndicator(col + 1, state)
          readInput(col + 1, state)
        } else {
          readInput(col, state)
        }                           
      case KeyType.Enter =>
        makeMove(col, state)
      case _ =>

    }
  }

  def gameOverMessage(winningPiece: GamePiece, state: GameState): Unit = {
    val image = new BasicTextImage(Size(state.board.width * 3 + state.board.width + 1,2))
    image.newTextGraphics().putString(Pos(0,0),s"Game Over, $winningPiece wins")
    image.newTextGraphics().putString(Pos(0,1),"Press anykey to exit")
    textGraphics.drawImage(Pos(5,state.board.height + 5),image)
    screen.refresh()
  }

  def makeMove(col: Int, state: GameState): Unit = {
    ConnectN.play(state, col, nextPiece(state)) match {
      case NextMove(newState, nextPiece) =>
        drawBoard(newState)
        drawIndicator(col,newState)
        screen.refresh()
        readInput(col, newState)
      case GameOver(newState, winningPiece) =>
        drawBoard(newState)
        emptyIndicator(newState)
        gameOverMessage(winningPiece,newState)
        screen.refresh()
        screen.readInput()
      case _ => 
    }
  }

  def drawBoard(state: GameState): TextImage = {
    val board = getBoardImage(state)
    textGraphics.drawImage(Pos(3,3),board)
    board
  }

  private def nextPiece(state: GameState): GamePiece =
    state.pieces.lastOption.map(_._2).getOrElse(Red).getOther

  def run(): Unit = {
    val state = GameState(Board(6, 6))
    val board = drawBoard(state)
    val height = board.getSize.getRows + 6
    val width = board.getSize.getColumns + 6
    val boarderSize = Size(width,height)
    textGraphics.drawRectangle(Pos(0,0),boarderSize,'X')
    drawIndicator(0, state)
    screen.refresh()
    readInput(0,state)
  }

  screen.startScreen()
  run()
  screen.stopScreen()
}
