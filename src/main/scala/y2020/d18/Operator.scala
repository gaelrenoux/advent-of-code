package y2020.d18

trait Operator {
  def apply(a: Long, b: Long): Long
}

object Plus extends Operator {
  override def apply(a: Long, b: Long): Long = a + b
}

object Mult extends Operator {
  override def apply(a: Long, b: Long): Long = a * b
}
