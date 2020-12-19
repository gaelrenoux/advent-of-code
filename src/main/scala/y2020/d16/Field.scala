package y2020.d16

case class Field(
    name: String,
    min1: Int,
    max1: Int,
    min2: Int,
    max2: Int
) {
  def isValid(i: Int): Boolean = {
    (i >= min1 && i <= max1) || (i >= min2 && i <= max2)
  }
}
