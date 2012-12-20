package de.ant.semantic.parser

import scala.util.parsing.combinator.JavaTokenParsers
import de.ant.semantic.formula.Formulas
import scalaz._, syntax.validation._, syntax.id._

/**
 * A simple parser for boolean algebra.
 */
trait FormulaParsers {

  this: Formulas =>

  /**
   * Parses the input and returns a formula if the input is valid or an error
   * message otherwise.
   *
   * @param in the input to parse
   *
   * @return a validation containing the formula or a error message
   */
  def parse(in: String): Validation[String, Formula] = {
    import parsers._

    def error(msg: String, input: Input): String = {
      val pos = input.pos.column - 1
      val xs = Seq(msg, "\n\t", input.source, "\n\t", " " * pos, "^")
      xs.mkString
    }

    parseAll(expr, in) match {
      case NoSuccess(msg, input) => error(msg, input).fail
      case Success(a, _)         => a.success
    }
  }

  /**
   * Internal object to keep the namespace clean.
   */
  private object parsers extends JavaTokenParsers {

    lazy val atom: Parser[Atom] =
      "[a-zA-Z]".r ^^ (Symbol(_) |> Atom)

    lazy val zero: Parser[_0] =
      "0" ^^ (_ => _0)

    lazy val one: Parser[_1] =
      "1" ^^ (_ => _1)

    lazy val expr: Parser[Formula] =
      term ~ rep("""[∨\+→↔⊕]""".r ~ term) ^^ {
        case f ~ Nil => f
        case f1 ~ (s ~ f2 :: fs) => (formula(f1, s, f2) /: fs) {
          case (f1, s ~ f2) => formula(f1, s, f2)
        }
      }

    def formula(f1: Formula, s: String, f2: Formula) = s match {
      case "∨" | "+" => f1 ∨ f2
      case "→"       => f1 → f2
      case "↔"       => f1 ↔ f2
      case "⊕"       => f1 ⊕ f2
    }

    lazy val term: Parser[Formula] =
      factor ~ rep(("""[∧\*]""".r | "") ~> factor) ^^ {
        case f ~ Nil => f
        case f ~ fs  => (f ∧ fs.head /: fs.tail) { (f1, f2) => f1 ∧ f2 }
      }

    lazy val factor: Parser[Formula] =
      opt("¬") ~ ("(" ~> expr <~ ")" | zero | one | atom) ^^ {
        case negation ~ expr => if (negation.isDefined) ¬(expr) else expr
      }
  }
}