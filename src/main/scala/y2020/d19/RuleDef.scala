package y2020.d19

import scala.util.matching.Regex

/*
trait Rule {
  def matches(s: String, ix: Int): Option[Int]
}

case class Character(c: Char) extends Rule {
  override def matches(s: String, ix: Int): Option[Int] =
    if (s.length > ix && s(ix) == c) Some(ix + 1)
    else None
}

case class Sequence(rs: Seq[Rule]) extends Rule {
  override def matches(s: String, ix: Int): Option[Int] = ???
}
*/

trait RuleDef

case class CharacterDef(c: Char) extends RuleDef {
  override def toString: String = s"CharacterDef($c)"
}

case class SequenceDef(ruleIds: Seq[Int]) extends RuleDef {
  override def toString: String = s"SequenceDef(${ruleIds.mkString(", ")})"
}

case class AlternativeDef(sequences: Seq[SequenceDef]) extends RuleDef {
  override def toString: String = s"AlternativeDef(${sequences.mkString(", ")})"
}

object RuleDef {
  def parse(content: String): RuleDef = {
    if (content.startsWith("\"")) {
      CharacterDef(content(1))
    } else {
      val orSequences = content.split("\\|").map(_.trim).map(seq => SequenceDef(seq.split(" ").map(_.trim.toInt)))
      if (orSequences.length == 1) orSequences.head
      else AlternativeDef(orSequences)
    }
  }
}
