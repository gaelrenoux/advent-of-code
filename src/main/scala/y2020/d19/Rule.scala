package y2020.d19

import scala.collection.mutable
import scala.util.matching.Regex

trait Rule {
  val toRegex: String

  lazy val toFullRegex = ("^" + toRegex + "$").r

  def matches(str: String): Boolean = toFullRegex.matches(str)
}

case class Character(c: Char) extends Rule {
  override def toString: String = s"Character($c)"

  override lazy val toRegex: String = c.toString
}

case class Sequence(rs: Seq[Rule]) extends Rule {
  override def toString: String = s"Sequence(${rs.mkString(", ")})"

  override lazy val toRegex: String = rs.map(_.toRegex).mkString("")
}

case class Alternative(rs: Seq[Rule]) extends Rule {
  override def toString: String = s"Alternative(${rs.mkString(", ")})"

  override lazy val toRegex: String = rs.map(_.toRegex).mkString("(", "|", ")")
}

object Rule {

  class Builder(definitions: Map[Int, RuleDef]) {
    private val rulesCache = mutable.Map[Int, Rule]()

    final def getRule(ix: Int): Rule = rulesCache.getOrElseUpdate(ix, definitions(ix) match {
      case CharacterDef(a) => Character(a)
      case SequenceDef(rids) => Sequence(rids.map(getRule))
      case AlternativeDef(seqDefs) => Alternative(seqDefs.map { case SequenceDef(rids) => Sequence(rids.map(getRule)) })
    })
  }
}
