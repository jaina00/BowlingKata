import scala.annotation.tailrec

sealed trait Frame {
  def score: Int

  def size: Int
}

sealed case class Normal(knock1: Int, knock2: Int) extends Frame {
  override def score: Int = knock1 + knock2

  override def size: Int = 2
}

sealed case class Spare(bonusKnock: Int) extends Frame {
  override def score: Int = 10 + bonusKnock

  override def size: Int = 2
}

sealed case class Strike(bonusKnock1: Int, bonusKnock2: Int) extends Frame {
  override def score: Int = 10 + bonusKnock1 + bonusKnock2

  override def size: Int = 1
}


object Tenpin extends App {

  @tailrec
  def pinsToFramesParser(remainingPins: List[Int], frames: List[Frame]): List[Frame] = {
    if (remainingPins.nonEmpty) {
      val newFrame = evaluateFrame(remainingPins)
      val newPins = remainingPins.drop(newFrame.size)
      pinsToFramesParser(newPins, frames :+ newFrame)
    } else
      frames
  }

  def evaluateFrame(remainingPins: List[Int]): Frame = {
    remainingPins match {
      case (10 :: bonus1 :: bonus2 :: _) => Strike(bonus1, bonus2)
      case (first :: second :: bonus :: _) if first + second == 10 => Spare(bonus)
      case (first :: second :: _) if first + second < 10 => Normal(first, second)
    }
  }

  def getScore(frames: List[Frame]): Int = frames.map(_.score).sum

  val score = Function.tupled(pinsToFramesParser _) andThen getScore

  val pinsKnocked: List[Int] = List(7, 2, 5, 5, 3, 0, 10, 2, 4)

  println(score(pinsKnocked, List.empty))

}