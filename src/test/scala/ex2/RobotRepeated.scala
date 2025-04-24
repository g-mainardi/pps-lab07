package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotRepeatedSpec extends AnyFlatSpec with Matchers:
  private def freshRobot: RobotRepeated = RobotRepeated(SimpleRobot((0, 0), Direction.North))

  "A Robot repeated" should "turn correctly" in :
    val robot = freshRobot
    val times: Int = 400

    robot.turn(Direction.East, times)
    robot.direction should be(Direction.East)

    robot.turn(Direction.South, times)
    robot.direction should be(Direction.South)

    robot.turn(Direction.West, times)
    robot.direction should be(Direction.West)

    robot.turn(Direction.North, times)
    robot.direction should be(Direction.North)

  it should s"act correctly with different times" in :
    val robot = freshRobot
    val northTimes: Int = 400
    val eastTimes: Int = 320
    val westTimes: Int = 270
    val southTimes: Int = 550

    robot.act(northTimes)
    robot.position should be((0, northTimes))

    robot.turn(Direction.East)
    robot.act(eastTimes)
    robot.position should be((eastTimes, northTimes))

    robot.turn(Direction.South)
    robot.act(southTimes)
    robot.position should be((eastTimes, northTimes - southTimes))

    robot.turn(Direction.West)
    robot.act(westTimes)
    robot.position should be((eastTimes - westTimes, northTimes - southTimes))