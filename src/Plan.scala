package offGridOrcs

import scala.util.Random

final case class Plan(steps: Seq[Step]) {
  def head = steps.head
  def tail = Plan(steps.tail)
}

object Plan {
  def idle(currentTime: Time): Plan = {
    val directions = Seq(
      Vec2(-1, 0),
      Vec2(+1, 0),
      Vec2(0, -1),
      Vec2(0, +1))
    val delays = Timings.IdleSequence
    val loops = 3
    def repeat[A] = Seq.fill(loops)(_: Seq[A]).flatten
    val directionSequence = Random.shuffle(repeat(directions))
    val arrivalTimeSequence = Random.shuffle(repeat(delays))
        .scanLeft(currentTime)(_ + _)
    val steps = directionSequence
      .zip(arrivalTimeSequence)
      .map(Function.tupled(Step.Walk))
    Plan(steps)
  }
}
