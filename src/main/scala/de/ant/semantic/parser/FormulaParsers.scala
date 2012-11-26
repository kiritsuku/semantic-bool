package de.ant.semantic.parser

import scala.util.parsing.combinator.JavaTokenParsers
import de.ant.semantic.formula.Formulas
import scalaz._, syntax.validation._, syntax.id._

trait FormulaParsers extends JavaTokenParsers {

  self: Formulas =>

  def parseFormula(in: String): Validation[String, Formula] = {
    parseAll(all, in) match {
      case NoSuccess(msg, input) => error(msg, input).fail
      case Success(a, _) => a.success
    }
  }

  private def error(msg: String, input: Input): String = {
    val pos = input.pos.column-1
    val xs = Seq(msg, "\n\t", input.source, "\n\t", " "*pos, "^")
    xs.mkString
  }

  lazy val atom: Parser[Atom] =
    "[a-zA-Z]".r ^^ (Symbol(_) |> Atom)

  lazy val zero: Parser[_0] =
    "0" ^^ (_ => _0)

  lazy val one: Parser[_1] =
    "1" ^^ (_ => _1)

  lazy val negation: Parser[¬] =
    "¬" ~> atom ^^ ¬

  lazy val natives: Parser[Formula] =
    negation | atom | zero | one

  lazy val forms: Parser[Formula] = (
      atom ~ atom ^^ { case a1 ~ a2 => a1 ∧ a2 }
    | atom ~ """[∧∨→↔⊕*\+]""".r ~ atom ^^ {
      case a1 ~ "∧" ~ a2 => a1 ∧ a2
      case a1 ~ "*" ~ a2 => a1 ∧ a2
      case a1 ~ "∨" ~ a2 => a1 ∨ a2
      case a1 ~ "+" ~ a2 => a1 ∨ a2
      case a1 ~ "→" ~ a2 => a1 → a2
      case a1 ~ "↔" ~ a2 => a1 ↔ a2
      case a1 ~ "⊕" ~ a2 => a1 ⊕ a2
    })

  lazy val all: Parser[Formula] =
    forms | natives
}