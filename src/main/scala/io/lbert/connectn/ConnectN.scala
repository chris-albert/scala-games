package io.lbert.connectn

object ConnectN {

  sealed trait GamePiece {
    def getOther: GamePiece = this match {
      case Red   => Black
      case Black => Red
    }
  }
  case object Red extends GamePiece
  case object Black extends GamePiece

  sealed trait MoveOutcome
  case class InvalidMove(state: GameState, piece: GamePiece, column: Int) extends MoveOutcome
  case class NextMove(state: GameState, nextPiece: GamePiece) extends MoveOutcome
  case class GameOver(state: GameState, winningPiece: GamePiece) extends MoveOutcome

  case class Board(height: Int, width: Int, inARow: Int = 4)

  case class Coord(x: Int, y: Int)

  case class GameState(board: Board, pieces: Seq[(Coord,GamePiece)] = Seq())

  def play(state: GameState, column: Int, piece: GamePiece): MoveOutcome =
    placePiece(state,column,piece) match {
      case None => InvalidMove(state, piece, column)
      case Some(newState) if isGameOver(newState) =>
        GameOver(newState, piece)
      case Some(newState) =>
        NextMove(newState, piece.getOther)
    }

  def placePiece(state: GameState, column: Int, piece: GamePiece): Option[GameState] =
    if(column < state.board.width) {
      getNextRow(state, column).map(nextRow =>
        state.copy(pieces = state.pieces ++ Seq(Coord(column, nextRow) -> piece))
      )
    } else None

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
