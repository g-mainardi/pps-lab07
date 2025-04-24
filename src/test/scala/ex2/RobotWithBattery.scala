package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotWithBatterySpec extends AnyFlatSpec with Matchers:
  val initialBattery = 15
  val turnCost = 1
  val actCost = 3

  private def freshRobot: RobotWithBattery = RobotWithBattery(SimpleRobot((0, 0), Direction.North), initialBattery)

  "A Robot with battery" should "turn correctly" in :
    val robot = freshRobot

    robot.turn(Direction.East, turnCost)
    robot.direction should be(Direction.East)

    robot.turn(Direction.South, turnCost)
    robot.direction should be(Direction.South)

    robot.turn(Direction.West, turnCost)
    robot.direction should be(Direction.West)

    robot.turn(Direction.North, turnCost)
    robot.direction should be(Direction.North)

  it should "act correctly" in :
    val robot = freshRobot

    robot.act(actCost)
    robot.position should be((0, 1))

    robot.turn(Direction.East, turnCost)
    robot.act(actCost)
    robot.position should be((1, 1))

    robot.turn(Direction.South, turnCost)
    robot.act(actCost)
    robot.position should be((1, 0))

    robot.turn(Direction.West, turnCost)
    robot.act(actCost)
    robot.position should be((0, 0))

  it should "consume battery correctly" in :
    val robot = freshRobot
    val actTimes = 3

    1 to actTimes foreach (_ => robot.act(actCost))
    robot.batteryLevel should be(initialBattery - actTimes * actCost)

  it should "do nothing when battery is over" in :
    val robot = freshRobot

    1 to initialBattery foreach (_ => robot.act(1))
    robot.position should be((0, initialBattery))
    robot.direction should be(Direction.North)
    robot.batteryLevel should be(0)

    robot.turn(Direction.South, turnCost)
    robot.position should be((0, initialBattery))
    robot.direction should be(Direction.North)
    robot.batteryLevel should be(0)
