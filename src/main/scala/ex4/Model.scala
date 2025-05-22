package ex4

import ex4.ConnectThreeElements.{Player, Board, emptyBoard, Disk}
import ex4.ConnectThreeFunctions.firstAvailableRow

class Model(starting: Player = Player.X) {
  private var _board: Board = emptyBoard
  private var _currentPlayer: Player = starting

  def board: Board = _board
  def currentPlayer: Player = _currentPlayer

  def reset(starting: Player = starting): Unit =
    _board = emptyBoard
    _currentPlayer = starting

  def dropDisk(x: Int): Option[Int] =
    firstAvailableRow(_board, x) match {
      case Some(y) =>
        placeDisk(x, y)
        switchPlayer()
        Some(y)
      case None => None
    }

  def winner: Option[Player] = _board.winner

  private def switchPlayer(): Unit = _currentPlayer = _currentPlayer.other
  private def placeDisk(x: Int, y: Int): Unit = _board = _board :+ Disk(x, y, _currentPlayer)
}
