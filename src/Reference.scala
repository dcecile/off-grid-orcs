package offGridOrcs

sealed trait Reference

object Reference {
  final case class Orc(index: Int) extends Reference
}
