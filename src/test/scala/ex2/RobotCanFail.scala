package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotCanFailSpec extends AnyFlatSpec with Matchers:
  private def freshRobot: RobotCanFail = RobotCanFail(SimpleRobot((0, 0), Direction.North))

  "A Robot that can fail" should "never fail to turn correctly" in :
    val robot = freshRobot
    val failProb = 0.0

    robot.turn(Direction.East, failProb)
    robot.direction should be(Direction.East)

    robot.turn(Direction.South, failProb)
    robot.direction should be(Direction.South)

    robot.turn(Direction.West, failProb)
    robot.direction should be(Direction.West)

    robot.turn(Direction.North, failProb)
    robot.direction should be(Direction.North)

  it should "never fail to act correctly" in :
    val robot = freshRobot
    val failProb = 0.0

    robot.act(failProb)
    robot.position should be((0, 1))

    robot.turn(Direction.East, failProb)
    robot.act(failProb)
    robot.position should be((1, 1))

    robot.turn(Direction.South, failProb)
    robot.act(failProb)
    robot.position should be((1, 0))

    robot.turn(Direction.West, failProb)
    robot.act(failProb)
    robot.position should be((0, 0))

  it should "act correctly sometimes" in :
    val robot = freshRobot
    val failProb = 0.5
    val attempts = 1000

    val possiblePositions = 0 to attempts map {(0, _)}
    possiblePositions.foreach(_ => robot.act(failProb))
    possiblePositions should contain (robot.position)

  it should "launch illegal argument exception" in :
    val robot = freshRobot

    an[IllegalArgumentException] should be thrownBy robot.turn(Direction.East, 3.0)
    an[IllegalArgumentException] should be thrownBy robot.turn(Direction.East, -0.5)
    an[IllegalArgumentException] should be thrownBy robot.turn(Direction.East, 100)

  it should "always fail" in :
    val robot = freshRobot
    val failProb = 1.0

    def testFail(): Unit =
      robot.turn(Direction.East, failProb)
      robot.direction should be(Direction.North)
      robot.act(failProb)
      robot.position should be((0, 0))

    1 to 100 foreach(_ => testFail())