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


  println(render(solution = Seq((0, 0), (2, 1)), board(width = 3, height = 3)))