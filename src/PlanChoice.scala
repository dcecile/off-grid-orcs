package offGridOrcs

sealed trait PlanChoice

object PlanChoice {
  final case class First(choices: Seq[PlanChoice]) extends PlanChoice
  def First(choices: PlanChoice*): First =
    First(choices: Seq[PlanChoice])

  final case class Best(choices: Seq[PlanChoice]) extends PlanChoice
  def Best(choices: PlanChoice*): Best =
    Best(choices: Seq[PlanChoice])

  final case class If(partialCondition: () => Boolean, choice: PlanChoice) extends PlanChoice {
    lazy val condition: Boolean = partialCondition()
  }
  def If(partialCondition: => Boolean, choice: PlanChoice): PlanChoice.If =
    If(() => partialCondition, choice)

  final case class One(partialPlan: () => Option[Plan]) extends PlanChoice {
    lazy val plan: Option[Plan] = partialPlan()
  }
  def apply(partialPlan: => Option[Plan]): PlanChoice.One =
    One(() => partialPlan)
}
