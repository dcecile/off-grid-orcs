package offGridOrcs

final case class Plan(preconditions: Seq[Precondition], steps: Seq[Step]) {
  def head = steps.head
  def tail = this.copy(steps = steps.tail)
  def isEmpty = steps.isEmpty
}

object Plan {
  val Zero = Plan(Seq(), Seq())
}
