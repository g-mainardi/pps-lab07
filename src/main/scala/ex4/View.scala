package ex4

import ex4.ConnectThreeElements.{Board, Player, emptyBoard}
import ex4.ConnectThreeFunctions.find

import java.awt.*
import java.awt.event.*
import javax.swing.*

class BoardPanel(cols: Int, rows: Int) extends JPanel {
  import Color.*
  private var _board: Board = emptyBoard

  def setBoard(b: Board): Unit = _board = b

  override def getPreferredSize = Dimension(400, 400)

  override def paintComponent(g: Graphics): Unit =
    super.paintComponent(g)
    val g2 = g.asInstanceOf[Graphics2D]
    drawBackground(g2)
    drawDiskPlaces(g2, cellWidth = getWidth / cols, cellHeight = getHeight / rows)

  private def drawDiskPlaces(g2: Graphics2D, cellWidth: Int, cellHeight: Int): Unit =
    for (x <- 0 until cols; y <- 0 until rows)
      val px = x * cellWidth
      val py = (rows - 1 - y) * cellHeight
      g2 setColor WHITE
      drawDisk(g2, cellWidth, cellHeight, px, py)
      find(_board, x, y)  foreach { p =>
        g2 setColor playerColor(p)
        drawDisk(g2, cellWidth, cellHeight, px, py)
      }

  private def drawDisk(g2: Graphics2D, cellWidth: Int, cellHeight: Int, px: Int, py: Int): Unit =
    g2 fillOval(px + 5, py + 5, cellWidth - 10, cellHeight - 10)

  private def playerColor(p: Player): Color = p match
      case Player.X => RED
      case _        => YELLOW

  private def drawBackground(g2: Graphics2D): Unit =
    g2 setColor GRAY
    g2 fillRect(0, 0, getWidth, getHeight)
}

class View(size: Int) extends JFrame("Connect Three") {
  private val cols = size + 1
  private val rows = size + 1
  private val buttonPanel = JPanel(GridLayout(1, cols))
  val board = BoardPanel(cols, rows)

  def initDiskPlaces(a: Int => Unit): Unit =
    for (x <- 0 until cols)
      val btn = JButton(s"Column $x")
      btn addActionListener (_ => a(x))
      buttonPanel add btn

  def displayMessage(msg: String): Unit =
    JOptionPane showMessageDialog(this, msg)

  def updateBoard(b: Board): Unit =
    board setBoard b
    board.repaint()

  getContentPane add(board, BorderLayout.CENTER)
  getContentPane add(buttonPanel, BorderLayout.NORTH)
  pack()
  setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  setLocationRelativeTo(null)
}

