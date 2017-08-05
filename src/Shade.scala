package offGridOrcs

sealed trait Shade
object Shade {
  final case class None() extends Shade
  final case class Highlight() extends Shade
  final case class Shadow() extends Shade
}
