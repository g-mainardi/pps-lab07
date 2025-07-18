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
  private def doAction(a: => Unit, cost: Int): Unit = if batteryLevel > 0 then {a; batteryLevel -= cost}
  def turn(dir: Direction, cost: Int): Unit = doAction(robot.turn(dir), cost)
  def act(cost: Int): Unit = doAction(robot.act(), cost)

class RobotCanFail(val robot: Robot):
  export robot.{position, direction}
  private val rand = scala.util.Random()
  private def checkProb(prob: Double): Unit = if !(prob >= 0 && prob <= 1) then 
    throw IllegalArgumentException("Probability must be between 0.0 and 1.0")
  private def doAction(a: => Unit, prob: Double): Unit = {checkProb(prob); if rand.nextDouble() > prob then a}
  def turn(dir: Direction, prob: Double): Unit = doAction(robot.turn(dir), prob)
  def act(prob: Double): Unit = doAction(robot.act(), prob)

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
