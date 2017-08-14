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
      case Precondition.CanCarryMore() =>
        orc.canCarryMore
    }
  }

  def choosePlan(orc: Orc, world: World): Plan = {
    def choices = PlanChoice.First(
      PlanChoice.If(
        !orc.canCarryMore,
        designDeliveryPlan(orc, world)),
      designBuildPlan(orc, world),
      PlanChoice.If(
        orc.isCarrying,
        designDeliveryPlan(orc, world)))

    choosePlan(choices)
      .getOrElse(designIdlePlan(orc, world))
  }

  def choosePlan(planChoice: PlanChoice): Option[Plan] = {
    planChoice match {
      case PlanChoice.First(choices) =>
        choices
          .toStream
          .map(choosePlan)
          .flatten
          .headOption
      case PlanChoice.Best(choices) =>
        val plans = choices
          .map(choosePlan)
          .flatten
        plans match {
          case Nil => None
          case _ => Some(plans.minBy(plan =>
            plan.steps.last.completionTime.frameNumber))
        }
      case conditional: PlanChoice.If =>
        if (conditional.condition) {
          choosePlan(conditional.choice)
        } else {
          None
        }
      case one: PlanChoice.One =>
        one.plan
    }
  }

  def designDeliveryPlan(orc: Orc, world: World): PlanChoice = {
    def subplan = designDestinationPlan(orc, world, Seq())(_, _, _)
    PlanChoice.First(
      subplan(
        world.activeGoals.flatMap(_.stockpilePositions),
        Timings.DropSpeed, Step.DropStock),
      subplan(
        Seq(orc.position),
        Timings.DropSpeed, Step.DropStock))
  }

  def designBuildPlan(orc: Orc, world: World): PlanChoice = {
    PlanChoice.Best(
      world.activeGoals.map(
        designBuildPlan(orc, world, _)))
  }

  def designBuildPlan(orc: Orc, world: World, goal: Goal): PlanChoice = {
    val preconditions = Seq(Precondition.CanCarryMore())
    def subplan = designDestinationPlan(orc, world, preconditions)(_, _, _)
    PlanChoice.First(
      subplan(
        goal.toClearPositions
          .intersect(goal.stockpilePositions),
        Timings.ChopWoodSpeed,
        Step.ChopWood),
      subplan(
        goal.toClearPositions,
        Timings.ChopWoodSpeed,
        Step.ChopWood),
      PlanChoice.If(
        orc.isCarrying,
        designDeliveryPlan(orc, world)),
      subplan(
        goal.toBuildFlooringPositions,
        Timings.BuildSpeed,
        Step.BuildFlooring),
      subplan(
        goal.toBuildWallsPositions,
        Timings.BuildSpeed,
        Step.BuildWalls),
      subplan(
        goal.toBuildRoofPositions,
        Timings.BuildSpeed,
        Step.BuildRoof),
      subplan(
        goal.toAddDecalPositions,
        Timings.BuildSpeed,
        Step.AddDecal))
  }

  def designDestinationPlan(orc: Orc, world: World, preconditions: Seq[Precondition])(positions: Seq[Vec2], stepSpeed: Duration, partialStep: Time => Step): PlanChoice =
    PlanChoice.If(
      positions.nonEmpty,
      PlanChoice({
        val destination = pickNearest(orc, positions)
        Some(Plan(
          preconditions,
          walkTo(destination, orc, world)
            .followedBy(stepSpeed, partialStep)))
      }))

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

  def designIdlePlan(orc: Orc, world: World): Plan = {
    val currentPosition = orc.position
    val currentTime = world.currentTime
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
