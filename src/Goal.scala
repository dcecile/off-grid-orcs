package offGridOrcs

final case class Goal(id: Reference.Goal, topLeft: Vec2, blueprint: Blueprint, startTime: Time, allPositions: Seq[Vec2], toClearPositions: Seq[Vec2], toBuildFlooringPositions: Seq[Vec2], toBuildWallsPositions: Seq[Vec2], toBuildRoofPositions: Seq[Vec2], toAddDecalPositions: Seq[Vec2], stockpilePositions: Seq[Vec2]) {
  val color = Colors.Goal
  val pulse = Pulse(
    startTime, Timings.GoalPulse, Colors.GoalPulseStart, 1.0, Pulse.Sine())
  val isActive = Seq(
    toClearPositions,
    toBuildFlooringPositions,
    toBuildWallsPositions,
    toBuildRoofPositions,
    toAddDecalPositions).exists(_.nonEmpty)
  def fromBlueprint(world: World): Goal =
    Goal.fromBlueprint(id, topLeft, blueprint, startTime, world)
}

object Goal {
  def fromBlueprint(id: Reference.Goal, topLeft: Vec2, blueprint: Blueprint, startTime: Time, world: World): Goal = {
    def translate(positions: Blueprint => Seq[Vec2]) =
      positions(blueprint).map(topLeft + _)
    def check(positions: Blueprint => Seq[Vec2], test: PartialFunction[Tile.Structure, Unit]) =
      translate(positions)
        .filter({ position =>
          val tile = world(position)
          test.isDefinedAt(tile.structure)
        })
    Goal(
      id,
      topLeft,
      blueprint,
      startTime,
      allPositions = translate(
        _.clearingPositions),
      toClearPositions = check(
        _.clearingPositions,
        { case Tile.Trees(_) => () }),
      toBuildFlooringPositions = check(
        _.buildingPositions,
        { case Tile.Grass(_) => () }),
      toBuildWallsPositions = check(
        _.buildingPositions,
        { case Tile.Building(Tile.Flooring()) => () }),
      toBuildRoofPositions = check(
        _.buildingPositions,
        { case Tile.Building(Tile.Walls()) => () }),
      toAddDecalPositions = check(
        _.decalPositions,
        { case Tile.Building(Tile.Roof()) => () }),
      stockpilePositions = translate(
        _.stockpilePositions))
  }
}
