package ex4

import scala.annotation.tailrec

object ConnectThreeElements {
  val marksToWin = 3
  val bound = 3

  enum Player:
    case X, O

    def other: Player = this match
      case X => O
      case _ => X

  case class Disk(x: Int, y: Int, player: Player)

  /**
   * Board:
   * y
   *
   * 3 _ _ _ _
   *
   * 2 _ _ _ _
   *
   * 1 _ _ _ _
   *
   * 0 _ _ _ _
   *
   *  >0 1 2 3 <-- x
   */
  type Board = Seq[Disk]
  type Game = Seq[Board]

  def emptyBoard: Board = Seq()
  def newGame: Game = Seq(emptyBoard)

  opaque type Axis = (Int, Int) => (Int, Int)
  private val axis_X: Axis = (x, y) => (y, x)
  private val axis_Y: Axis = (_, _)

  extension (board: Board)
    private def maxConsecutive(seq: Seq[Int]): Int =
      seq.foldLeft((0, 0)) { case ((mx, cur), v) =>
        val c = if v == 1 then cur + 1 else 0
        (mx max c, c)
      }._1
    private def positions(player: Player): Set[(Int, Int)] =
      board.collect { case c if c.player == player => (c.x, c.y) }.toSet
    private def maxStreakOn(player: Player, axis: Axis): Seq[Int] =
      val pos = board positions player
      0 to bound map { y => maxConsecutive(0 to bound map { x => if pos(axis(x, y)) then 1 else 0 }) }
    def maxStreakOnRows(player: Player): Seq[Int] = maxStreakOn(player, axis_Y)
    def maxStreakOnCols(player: Player): Seq[Int] = maxStreakOn(player, axis_X)
    def maxStreak(player: Player): Int = Seq(board.maxStreakOnRows(player).max, board.maxStreakOnCols(player).max).max
    def winner: Option[Player] = Player.values find {maxStreak(_) >= marksToWin}

  extension (game: Game)
    def lastWinner: Option[Player] = game.last.winner
    def endPrint(): Unit =
      val winner = game.lastWinner
      println(if winner.isDefined then s"Game over, the winner is ${winner.get}" else "Game is not over")
}

object ConnectThreeFunctions {

  import ConnectThreeElements.*

  def find(board: Board, x: Int, y: Int): Option[Player] = board.find(d => (d.x == x) && (d.y == y)).map(_.player)

  def firstAvailableRow(board: Board, x: Int): Option[Int] =
    (for
      y <- 0 to bound
      if find(board, x, y).isEmpty
    yield y).headOption

  def placeAnyDisk(board: Board, player: Player): Seq[Board] =
    for
      x <- 0 to bound
      y <- firstAvailableRow(board, x)
    yield
      board :+ Disk(x, y, player)

  def computeAnyGame(player: Player, moves: Int): LazyList[Game] = moves match
    case 0 => LazyList(newGame)
    case _ =>
      for
        game      <- computeAnyGame(player.other, moves - 1)
        new_board <- placeAnyDisk(game.last, player)
      yield
        game :+ new_board

  def computeAnyGameStopping(player: Player, moves: Int): LazyList[Game] = moves match
    case 0 => LazyList(newGame)
    case _ =>
      for
        game      <- computeAnyGameStopping(player.other, moves - 1)
        new_board <- placeAnyDisk(game.last, player)
      yield
        if game.lastWinner.isEmpty then 
          game :+ new_board
        else 
          game

  def printBoards(game: Seq[Board]): Unit =
    for
      y     <- bound to 0 by -1
      board <- game.reverse
      x     <- 0 to bound
    do
      print(find(board, x, y) map(_.toString) getOrElse ".")
      (x, game.head) match
        case (`bound`, `board`) => println("\t")
        case (`bound`, _)       => print("\t")
        case _                  => ()

  def printGame(g: Game): Unit =
    printBoards(g)
    g.endPrint()

  import scala.util.Random
  val rand = Random(42)
  def randomAi(board: Board, side: Player): Board =
    val avPos: Seq[(Int, Int)] = for
      x <- 0 to bound
      y <- firstAvailableRow(board, x)
    yield
      (x, y)
    val randPos: (Int, Int) = avPos(rand nextInt avPos.size)
    board :+ Disk(randPos._1, randPos._2, side)
}

object ConnectThree extends App:

  import ConnectThreeElements.*
  import Player.*
  import ConnectThreeFunctions.*
  // Exercise 1: implement find such that..
  println("EX 1: ")
  println(find(List(Disk(0, 0, X)), 0, 0)) // Some(X)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 0, 1)) // Some(O)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 1, 1)) // None

  // Exercise 2: implement firstAvailableRow such that..
  println("EX 2: ")
  println(firstAvailableRow(List(), 0)) // Some(0)
  println(firstAvailableRow(List(Disk(0, 0, X)), 0)) // Some(1)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X)), 0)) // Some(2)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X)), 0)) // Some(3)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X)), 0)) // None
  // Exercise 2: implement placeAnyDisk such that..
  printBoards(placeAnyDisk(List(), X))
  // .... .... .... ....
  // .... .... .... ....
  // .... .... .... ....
  // ...X ..X. .X.. X...
  printBoards(placeAnyDisk(List(Disk(0, 0, O)), X))
  // .... .... .... ....
  // .... .... .... ....
  // ...X .... .... ....
  // ...O ..XO .X.O X..O
  println("EX 4: ")
  // Exercise 3 (ADVANCED!): implement computeAnyGame such that..
  computeAnyGame(O, 4) foreach { g => {printBoards(g); println}}
  // .... .... .... .... ...O
  // .... .... .... ...X ...X
  // .... .... ...O ...O ...O
  // .... ...X ...X ...X ...X
  //
  //
  // .... .... .... .... O...
  // .... .... .... X... X...
  // .... .... O... O... O...
  // .... X... X... X... X...

  // Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
  computeAnyGameStopping(O, 8) foreach { g => {printBoards(g); println}}
  // OO..
  // XX..
  // OO..
  // XX.X
  val board: Board = List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X), Disk(0, 3, O), Disk(1, 0, X), Disk(1, 1, O), Disk(1, 2, X), Disk(1, 3, O), Disk(3, 0, X))
  println(s"Board state: \n $board")

  private val game: Game = Seq(board)
  printGame(game)

  private val endGame: Game = Seq(board :+ Disk(2, 0, X))
  printGame(endGame)

  @tailrec
  private def playRandomAi(board: Board, player: Player): Unit = board.winner match
    case Some(w) => printBoards(Seq(board));println(s"The winner is $w")
    case _       => playRandomAi(randomAi(board,player), player.other)

  playRandomAi(emptyBoard, X)
  playRandomAi(emptyBoard, X)
  playRandomAi(emptyBoard, X)
