package offGridOrcs

import offGridOrcs.StepExtensions._
import scala.util.Random

object AI {
  def reevaluatePlan(orc: Orc, world: World): Orc = {
    val isPlanInvalid = orc.plan.isEmpty || isPlanPreconditionInvalid(orc, world)
    if (isPlanInvalid) {
      orc.copy(plan =
        choosePlan(orc, world))
    } else {
      orc
    }
  }

  def isPlanPreconditionInvalid(orc: Orc, world: World): Boolean = {
    orc.plan.preconditions.exists(
      !isPlanPreconditionValid(_, orc, world))
  }

  def isPlanPreconditionValid(precondition: Precondition, orc: Orc, world: World): Boolean = {
    precondition match {
      case Precondition.UnobstructedPath() =>
        true
      case Precondition.NoGoals() =>
        world.activeGoals.isEmpty
    }
  }

  def choosePlan(orc: Orc, world: World): Plan = {
    val plan = world.activeGoals.headOption match {
      case Some(goal) =>
        val destination = pickNearest(
          orc, goal.toClearPositions)
        Some(Plan(
          Seq(),
          walkTo(destination, orc, world).followedBy(Timings.ChopWoodSpeed, Step.ChopWood(goal.id, _))))
      case None =>
        None
    }
    plan.getOrElse(
      buildIdlePlan(orc.position, world.currentTime))
  }

  def pickNearest(orc: Orc, positions: Seq[Vec2]): Vec2 = {
    Random.shuffle(positions).minBy(position =>
      (position - orc.position).gridLength)
  }

  def walkTo(finalDestination: Vec2, orc: Orc, world: World): Seq[Step] = {
    val diff = finalDestination - orc.position
    val directionX = Seq.fill(diff.x.abs.toInt)(Vec2(diff.x.signum.toDouble, 0))
    val directionY = Seq.fill(diff.y.abs.toInt)(Vec2(0, diff.y.signum.toDouble))
    val directionSequence = Random.shuffle(directionX ++ directionY)
    zipWalk(
      orc.position,
      world.currentTime,
      directionSequence,
      Seq.fill(directionSequence.length)(Timings.WalkSpeed))
  }

  def buildIdlePlan(currentPosition: Vec2, currentTime: Time): Plan = {
    val directions = Vec2.FourDirections
    val delays = Timings.IdleSequence
    val loops = 3
    def repeat[A] = Seq.fill(loops)(_: Seq[A]).flatten
    def steps = zipWalk(
      currentPosition,
      currentTime,
      Random.shuffle(repeat(directions)),
      Random.shuffle(repeat(delays)))
    Plan(
      Seq(Precondition.NoGoals(), Precondition.UnobstructedPath()),
      steps)
  }

  def zipWalk(currentPosition: Vec2, currentTime: Time, directionSequence: Seq[Vec2], delaySequence: Seq[Duration]): Seq[Step.Walk] = {
    val destinationSequence = directionSequence
      .scanLeft(currentPosition)(_ + _)
    val completionTimeSequence = delaySequence
      .scanLeft(currentTime)(_ + _)
    destinationSequence
      .zip(completionTimeSequence)
      .map(Function.tupled(Step.Walk))
  }
}
