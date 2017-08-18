package io.lbert

import java.nio.charset.Charset
import com.googlecode.lanterna._
import com.googlecode.lanterna.input.KeyType
import com.googlecode.lanterna.screen.TerminalScreen
import com.googlecode.lanterna.terminal.DefaultTerminalFactory
import com.googlecode.lanterna.graphics.BasicTextImage

trait LaternaUI {

  val terminal = new DefaultTerminalFactory(System.out, System.in, Charset.forName("UTF8")).createTerminal()
  val screen = new TerminalScreen(terminal)
  val termSize = terminal.getTerminalSize
  val textGraphics = screen.newTextGraphics()

  def run(func: => Unit): Unit = {
    screen.startScreen()
    func
    screen.stopScreen()
  }
}

object LaternaUI {
  case class Pos(x: Int,y: Int) extends TerminalPosition(x,y) {
    def up: Pos = copy(y = y - 1)
    def down: Pos = copy(y = y + 1)
    def left: Pos = copy(x = x - 1)
    def right: Pos = copy(x = x + 1)
    def toCoord: Coord = Coord(x,y)
  }
  object Pos {
    def apply(tp: TerminalPosition): Pos = Pos(tp.getColumn,tp.getRow)
    def apply(coord: Coord): Pos = Pos(coord.x,coord.y)
    def offset(pos1: Pos, pos2: Pos): Pos =
      Pos(pos1.x - pos2.x, pos1.y - pos2.y)
  }
  case class Size(cols: Int, rows: Int) extends TerminalSize(cols,rows)
  case class Rectangle(pos: Pos, size: Size) {
    lazy val grid = Grid(size.rows,size.cols)
    def inGrid(pos: Pos): Boolean =
      Grid.isBounded(grid,Coord(pos.x - this.pos.x, pos.y - this.pos.y))
  }
  case class TextChar(char: Char,
                      foregroundColor: TextColor = TextColor.ANSI.DEFAULT,
                      backgroundColor: TextColor = TextColor.ANSI.DEFAULT)
    extends TextCharacter(char,foregroundColor,backgroundColor,java.util.EnumSet.noneOf(classOf[SGR]))

  case class TextImage(size: Size) extends BasicTextImage(size)

  object TextImage {
    def apply(grid: Grid): TextImage = TextImage(Size(grid.col,grid.row))
  }

  def inBoundsOption(bound: Rectangle, pos: Pos): Option[Pos] =
    if (bound.inGrid(pos)) Some(pos) else None

  def readBoundedInput(bounds: Rectangle, key: KeyType, curr: Pos): Option[Pos] = {
    key match {
      case KeyType.ArrowLeft  => inBoundsOption(bounds,curr.left)
      case KeyType.ArrowRight => inBoundsOption(bounds,curr.right)
      case KeyType.ArrowUp    => inBoundsOption(bounds,curr.up)
      case KeyType.ArrowDown  => inBoundsOption(bounds,curr.down)
      case _                  => None
    }
  }
}
