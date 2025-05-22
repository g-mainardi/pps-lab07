package ex4

import ex4.ConnectThreeFunctions.{AI, SmartAI, RandomAI}
import ex4.ConnectThreeElements.{Player, bound}
import Player.{O, X}

import javax.swing.SwingUtilities

object PlayGUI :
  /**
   * There are 3 game mode:
   * <li> Player vs Player
   * <li> Player vs AI (Smart or Random, passed as parameter)
   * <li> AI vs AI (passed as parameters)
   */
  def main(args: Array[String]): Unit = 
//    Play1Vs1().run()
//    Play1VsAi(SmartAI(X)).run()
    PlayAiVsAi(RandomAI(X, 711), SmartAI(O)).run()

trait Controller {
  protected val view = View(size = bound)
  protected var model = Model()
  def run(): Unit = SwingUtilities invokeLater (() => view setVisible true)
  protected def updateView(): Unit = view updateBoard model.board
  protected def winString(winner: Option[Player], current: Player): String
  protected object WinStatus:
    def unapply(tup: (Option[Player], Player)): Option[String] =
      val (winner, current) = tup
      if winner.nonEmpty then Some(winString(winner, current))
      else None
  protected def handleDrop(x: Int): Unit
}

abstract class UserInteractionController extends Controller {
  view initDiskPlaces handleDrop
  protected def handleDrop(x: Int, actions: (Option[Player], Player) => Unit): Unit =
    model dropDisk x match
      case Some(y) =>
        updateView()
        (model.winner, model.currentPlayer) match
          case WinStatus(winner) =>
            view displayMessage s"$winner wins!"
            model.reset()
            updateView()
          case (w, cp) => actions(w, cp)
      case None =>
        view displayMessage "Column full!"
}

class Play1Vs1 extends UserInteractionController {
  override def winString(winner: Option[Player], current: Player): String = s"Player ${current.other}"
  override def handleDrop(x: Int): Unit = handleDrop(x, (_, _) => ())
}

class Play1VsAi(ai: AI) extends UserInteractionController {
  model = Model(starting = ai.side.other)
  override def handleDrop(x: Int): Unit =
    val actions: (Option[Player], Player) => Unit = (_, _) match
      case (_, ai.side) => Thread sleep 800; handleDrop(ai play model.board)
      case _ => ()
    handleDrop(x, actions)
  protected def winString(winner: Option[Player], current: Player): String = current match
    case ai.side => s"Player $current"
    case _       => ai.toString
}

class PlayAiVsAi(ai1: AI, ai2: AI) extends Controller {
  assert(ai1.side != ai2.side)
  model = Model(starting = ai1.side)

  override def run(): Unit =
    super.run()
    handleDrop(currentAi play model.board)

  override def handleDrop(x: Int): Unit =
    Thread sleep 1500
    model dropDisk x match
      case Some(y) =>
        updateView()
        (model.winner, model.currentPlayer) match
          case WinStatus(winner) =>
            view displayMessage s"$winner wins!"
            model.reset()
            updateView()
          case _ => handleDrop(currentAi play model.board)
      case None =>
        println(s"Error dropping disk with [${model.currentPlayer}] in column [$x]")

  override def winString(winner: Option[Player], current: Player): String = currentAi.toString

  private def currentAi: AI = model.currentPlayer match
    case ai1.side => ai1
    case _        => ai2
}
