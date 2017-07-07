package io.lbert.connectn

import io.lbert.connectn.ConnectN._
import scala.util.{Failure, Success, Try}

object ConnectNASCII extends App {

  //ansi code
  case class ASCIIChars(blank: String = "X",
                        red: String = "R",
                        black: String = "B",
                        separator: String = "|")

  def draw(state: GameState, chars: ASCIIChars): Seq[String] = {
    val pieceMap = state.pieces.toMap
    (0 until state.board.height).map(y =>
      (0 until state.board.width).map{x =>
        val coord = Coord(x,y)
        pieceMap.get(coord).fold(chars.blank) {
          case Red => chars.red
          case Black => chars.black
        }
      }.mkString(s"${chars.separator} ", s" ${chars.separator} ", s" ${chars.separator}")
    ).reverse
  }

  def run(state: GameState): Unit = {
    val board = draw(state,ASCIIChars())
    println()
    println(board.mkString("\n"))
    println()
    val piece = nextPiece(state)
    print(s"Enter column for $piece: ")
    Try(scala.io.StdIn.readInt()) match {
      case Success(i) => ConnectN.play(state,i,piece).map(run)
      case Failure(e) =>
        println("Invalid input, try again")
        run(state)
    }
  }

  private def nextPiece(state: GameState): GamePiece =
    state.pieces.lastOption.map(_._2).getOrElse(Red) match {
      case Red   => Black
      case Black => Red
    }

  println("Welcome to Connect N")
  run(GameState(Board(4,4)))
}
