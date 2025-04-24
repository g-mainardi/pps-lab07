package ex2

type Position = (Int, Int)
enum Direction:
  case North, East, South, West
  def turnRight: Direction = this match
    case Direction.North => Direction.East
    case Direction.East => Direction.South
    case Direction.South => Direction.West
    case Direction.West => Direction.North

  def turnLeft: Direction = this match
    case Direction.North => Direction.West
    case Direction.West => Direction.South
    case Direction.South => Direction.East
    case Direction.East => Direction.North

trait Robot:
  def position: Position
  def direction: Direction
  def turn(dir: Direction): Unit
  def act(): Unit

class SimpleRobot(var position: Position, var direction: Direction) extends Robot:
  def turn(dir: Direction): Unit = direction = dir
  def act(): Unit = position = direction match
    case Direction.North => (position._1, position._2 + 1)
    case Direction.East => (position._1 + 1, position._2)
    case Direction.South => (position._1, position._2 - 1)
    case Direction.West => (position._1 - 1, position._2)

  override def toString: String = s"robot at $position facing $direction"

class DumbRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, act}
  override def turn(dir: Direction): Unit = {}
  override def toString: String = s"${robot.toString} (Dump)"

class LoggingRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    robot.act()
    println(robot.toString)

class RobotWithBattery(val robot: Robot, var batteryLevel: Int):
  export robot.{position, direction}
  private def canDoAction: Boolean = batteryLevel > 0
  private def consume(cost: Int): Unit = batteryLevel -= cost
  def turn(dir: Direction, cost: Int): Unit = if canDoAction then
    robot.turn(dir); consume(cost)
  def act(cost: Int): Unit = if canDoAction then
    robot.act(); consume(cost)

class RobotCanFail(val robot: Robot):
  export robot.{position, direction}
  import scala.util.Random
  private val rand = Random()
  private def checkProb(prob: Double): Unit = if !(prob >= 0 && prob <= 1) then 
    throw new IllegalArgumentException("Probability must be between 0.0 and 1.0")
  private def canDoAction(prob: Double): Boolean = 
    checkProb(prob); rand.nextDouble() > prob
  def turn(dir: Direction, prob: Double): Unit = if canDoAction(prob) then robot.turn(dir)
  def act(prob: Double): Unit = if canDoAction(prob) then robot.act()

class RobotRepeated(val robot: Robot):
  export robot.{position, direction, turn, act}
  def turn(dir: Direction, times: Int): Unit = 1 to times foreach(_ => turn(dir))
  def act(times: Int): Unit = 1 to times foreach(_ => act())

@main def testRobot(): Unit =
  val robot = LoggingRobot(SimpleRobot((0, 0), Direction.North))
  robot.act() // robot at (0, 1) facing North
  robot.turn(robot.direction.turnRight) // robot at (0, 1) facing East
  robot.act() // robot at (1, 1) facing East
  robot.act() // robot at (2, 1) facing East
