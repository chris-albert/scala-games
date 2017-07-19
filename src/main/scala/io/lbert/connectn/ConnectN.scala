package io.lbert.connectn

import io.lbert.Coord

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
    val startCoords = getStartCoords(board, coord)
    val sliceIterator = 0 until (board.inARow * 2) - 1

    Seq(
      sliceIterator.map(i => Coord(startCoords.up.x,startCoords.up.y + i)),
      sliceIterator.map(i => Coord(startCoords.right.x + i, startCoords.right.y)),
      sliceIterator.map(i => Coord(startCoords.upRight.x + i, startCoords.upRight.y + i)),
      sliceIterator.map(i => Coord(startCoords.downRight.x + i, startCoords.downRight.y - i))
    ).map(_.filter(isOffBoard(_,board))).filter(ifNotEnough(_,board))
  }

  def ifNotEnough(coords: Seq[Coord], board: Board): Boolean =
    coords.size >= board.inARow

  def isOffBoard(coord: Coord, board: Board): Boolean =
    coord.x >= 0 && coord.x < board.width && coord.y >= 0 && coord.y < board.height

  case class StartCoords(right: Coord,
                         up: Coord,
                         upRight: Coord,
                         downRight: Coord)

  def getStartCoords(board: Board, coord: Coord): StartCoords =
    StartCoords(
      right = Coord(coord.x - board.inARow + 1, coord.y),
      up = Coord(coord.x, coord.y - board.inARow + 1),
      upRight = Coord(coord.x - board.inARow + 1, coord.y - board.inARow + 1),
      downRight = Coord(coord.x - board.inARow + 1, coord.y + board.inARow - 1)
    )
}
