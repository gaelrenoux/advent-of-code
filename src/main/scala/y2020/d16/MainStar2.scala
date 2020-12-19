package y2020.d16

import scala.collection.mutable

// scalastyle:off
object MainStar2 extends App {

  val FieldDefRegex = """([^:]*): (\d+)-(\d+) or (\d+)-(\d+)""".r
  type Ticket = Array[Int]

  val input = Input.real

  val Array(fieldDefTxt, myTicketsTxt, otherTicketsTxt) = input.split("\n\n")
  val fieldDefLines = fieldDefTxt.split("\n").map(_.trim).filter(_.nonEmpty)
  val fieldDefinitions = fieldDefLines.map {
    case FieldDefRegex(name, min1, max1, min2, max2) => Field(name, min1.toInt, max1.toInt, min2.toInt, max2.toInt)
  }

  println(fieldDefinitions.toList)
  val fieldCount = fieldDefinitions.size

  val myTicket: Ticket = myTicketsTxt.split("\n").map(_.trim).filter(_.nonEmpty).tail.head.split(",").map(_.toInt)

  @inline private final def noRuleValidate(i: Int): Boolean = !fieldDefinitions.exists(_.isValid(i))

  val otherTickets: Array[Ticket] = otherTicketsTxt.split("\n").map(_.trim).filter(_.nonEmpty).tail.map(_.split(",").map(_.toInt))
  val validOtherTickets = otherTickets.filterNot(t => t.exists(noRuleValidate))

  val possibleFieldsPerPosition = mutable.Map[Int, mutable.Set[Field]]()
  (0 until fieldCount).foreach { i =>
    val possibilities = fieldDefinitions.filter { r =>
      validOtherTickets.map(t => t(i)).forall(r.isValid)
    }
    possibleFieldsPerPosition.put(i, mutable.Set(possibilities: _*))
  }

  val positionPerField = mutable.Map[Field, Int]()

  var i = 0

  while (possibleFieldsPerPosition.nonEmpty && i < 100) {
    i = i + 1
    val newlyAttributedFields = possibleFieldsPerPosition.filter(_._2.size == 1).map { case (k, v) => v.head -> k }
    positionPerField ++= newlyAttributedFields
    possibleFieldsPerPosition.subtractAll(newlyAttributedFields.values).foreach {
      case (_, ps) => ps.subtractAll(newlyAttributedFields.keys)
    }
  }

  if (i == 100) {
    throw new Exception("ARGH")
  }



  println(positionPerField.toList.map { case (k, v) => k.name -> v }.sortBy(_._2).mkString("\n", "\n", "\n"))
  println(myTicket.toList)

  val departureFields = positionPerField.view.filterKeys(_.name.startsWith("departure")).values.toList.sorted
  println(departureFields)
  val myDepartureFields = departureFields.map(myTicket.apply)
  println(myDepartureFields)
  println(myDepartureFields.map(_.toLong).product)

}
