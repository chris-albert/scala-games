package io.lbert.connectn

object ConnectN {

  sealed trait GamePiece
  case object Red extends GamePiece
  case object Black extends GamePiece

  case class Board(height: Int, width: Int, inARow: Int = 4)

  case class Coord(x: Int, y: Int)

  case class GameState(board: Board, pieces: Seq[(Coord,GamePiece)] = Seq())

  def play(state: GameState, column: Int, piece: GamePiece): Option[GameState] =
    getNextRow(state,column).map(nextRow =>
      state.copy(pieces = state.pieces ++ Seq(Coord(column,nextRow) -> piece))
    )

  def getNextRow(state: GameState, column: Int): Option[Int] = {
    val count = state.pieces.count(_._1.x == column)
    if(count < state.board.height) Some(count)
    else None
  }

  def isGameOver(state: GameState): Boolean =
    state.pieces.lastOption.exists { case (coord, piece) =>
      val slices = getSlices(state.board,coord)
      slices.map(isSliceWin(_,state,piece)).count(b => b) > 0
    }

  def isSliceWin(seq: Seq[Coord], state: GameState, piece: GamePiece): Boolean =
    seq.sliding(state.board.inARow).count(_.count(c => isPiece(piece,c,state)) == state.board.inARow) > 0

  def isPiece(piece: GamePiece, coord: Coord, state: GameState): Boolean =
    state.pieces.contains(coord -> piece)

  def getSlices(board: Board, coord: Coord): Seq[Seq[Coord]] = {
    val x = (0 until board.height).map(r => Coord(coord.x,r))
    val y = (0 until board.width).map(c => Coord(c, coord.y))
    Seq(x,y)
  }
}
