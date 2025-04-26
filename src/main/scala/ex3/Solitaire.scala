package ex3

object Solitaire extends App:
  object Elements {
    opaque type Board = (Int, Int)
    def board(width: Int, height: Int): Board = (width, height)
    extension (b: Board)
      def width: Int = b match
        case (w, _) => w
      def height: Int = b match
        case (_, h) => h
      def middle: Position = (b.width / 2, b.height / 2)
    type Position = (Int, Int)
    extension (p: Position)
      def +(q: Position): Position = (p._1 + q._1, p._2 + q._2)
    type Solution = Seq[Position]
  }
  import Elements.*
  def render(solution: Solution, b: Board): String =
    val reversed = solution//.reverse
    val rows =
      for y <- 0 until b.height
          row = for x <- 0 until b.width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")
  def isInside(b: Board, position: (Int, Int)): Boolean =
    (position._1 >= 0) && (position._1 <= b.width - 1) && (position._2 >= 0) && (position._2 <= b.height - 1)
  def isOccupied(mark1: Position, mark2: Position): Boolean = mark1 == mark2
  def isSafe(mark: Position, marks: Solution) =
    marks forall (!isOccupied(mark, _))
  val offsets: Set[Position] = for
      (x, y) <- Set((2, 2), (3, 0), (0, 3))        // 3 types of movements: diagonal, horizontal and vertical
      xi <- if x == 0 then Seq(0) else Seq(x, -x)
      yi <- if y == 0 then Seq(0) else Seq(y, -y)
  yield (xi, yi)
  def placeMarks(b: Board): LazyList[Solution] =
    def _placeNumbers(n: Int): LazyList[Solution] = n match
      case 1 => LazyList(LazyList(b.middle))
      case _ =>
        for
          numbers <- _placeNumbers(n - 1)
          offset <- offsets
          number = numbers.last + offset
          if isInside(b, number) && isSafe(number, numbers)
        yield
          numbers :+ number
    _placeNumbers(b.width * b.height)

  val b = board(width = 5, height = 5)
  val solutions: Seq[(Solution, Int)] = placeMarks(b).zipWithIndex

  // Print solution + render + index
  solutions.foreach(sol => println(s"${sol._1}\n${render(sol._1, b)}\nIndice: ${sol._2}\n"))
  println("Number of solutions: " + solutions.size)