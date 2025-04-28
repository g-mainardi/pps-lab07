package ex4

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ConnectThreeElementsTest extends AnyFlatSpec with Matchers {
  import ConnectThreeElements.*
  val sizeConnect: Int = bound + 1

  "maxStreakOnRows" should "return all zeros on an empty board" in :
    val b: Board = emptyBoard
    b.maxStreakOnRows(Player.X) shouldEqual Seq(0, 0, 0, 0)
    b.maxStreakOnRows(Player.O) shouldEqual Seq(0, 0, 0, 0)

  it should "count a single mark as streak=1" in :
    val b: Board = Seq(Disk(1, 1, Player.X))
    b.maxStreakOnRows(Player.X) shouldEqual Seq(0, 1, 0, 0)
    b.maxStreakOnCols(Player.X) shouldEqual Seq(0, 1, 0, 0)

  it should "detect horizontal consecutive marks" in :
    val row0 = 0 until sizeConnect map{ x => Disk(x, 0, Player.X) }
    val b: Board = row0
    b.maxStreakOnRows(Player.X) shouldEqual Seq(sizeConnect, 0, 0, 0)

  "maxStreakOnCols" should "detect vertical consecutive marks" in :
    val col3 = 0 until sizeConnect map{y => Disk(3, y, Player.O)}
    val b: Board = col3
    b.maxStreakOnCols(Player.O) shouldEqual Seq(0, 0, 0, sizeConnect)

  "maxStreak" should "return the maximum across rows and columns" in :
    val nMarks = 3
    val row0 = Seq(Disk(0, 0, Player.X))
    val col1 = 0 until nMarks map{y => Disk(1, y, Player.X)}
    val b: Board = row0 ++ col1
    b.maxStreak(Player.X) shouldEqual nMarks

  it should "return the consequent ones across rows and columns" in :
    val row0 = 0 until sizeConnect map { x => Disk(x, 0, Player.X) }
    val b: Board = row0 filter (_.x != 1)
    b.maxStreakOnRows(Player.X) shouldEqual Seq(2, 0, 0, 0)
    b.maxStreakOnCols(Player.X) shouldEqual Seq(1, 0, 1, 1)
    b.maxStreak(Player.X) shouldEqual 2

  it should "be zero if the player has no marks" in :
    val b: Board = Seq(Disk(0, 0, Player.O))
    b.maxStreak(Player.X) shouldEqual 0

  "isOver" should "return the winner if he connects three" in :
    val row0 = 0 until sizeConnect map { x => Disk(x, 0, Player.X) } filter (_.x != 1)
    val col1 = 0 until marksToWin map { y => Disk(1, y, Player.O)}
    val b: Board = row0 ++ col1
    b.winner shouldEqual Some(Player.O)
}
