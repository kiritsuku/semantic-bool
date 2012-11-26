package de.ant.semantic.parser

import scala.util.parsing.combinator.JavaTokenParsers
import de.ant.semantic.formula.Formulas
import scalaz._, syntax.validation._, syntax.id._

trait FormulaParsers extends JavaTokenParsers {

  self: Formulas =>

  def parseFormula(in: String): Validation[String, Formula] =
    parseAll(full, in) match {
      case NoSuccess(msg, input) => error(msg, input).fail
      case Success(a, _) => a.success
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

  lazy val and: Parser[∧] =
    binaryOp("")(∧) | binaryOp("[∧\\*]")(∧)

  lazy val or: Parser[∨] =
    binaryOp("[∨\\+]")(∨)

  lazy val implies: Parser[→] =
    binaryOp("→")(→)

  lazy val equivalence: Parser[↔] =
    binaryOp("↔")(↔)

  lazy val antivalence: Parser[⊕] =
    binaryOp("⊕")(⊕)

  lazy val all: Parser[Formula] =
    and | or | implies | equivalence | antivalence | natives

  lazy val paren: Parser[Formula] =
    "(" ~> all <~ ")"

  lazy val full: Parser[Formula] =
      all | paren

  private def error(msg: String, input: Input): String = {
    val pos = input.pos.column-1
    val xs = Seq(msg, "\n\t", input.source, "\n\t", " "*pos, "^")
    xs.mkString
  }

  private def binaryOp[A](rgx: String)(f: (A, Atom) => A): Parser[A] =
    atom ~ rep1(rgx.r ~> atom) ^^ {
      case a ~ xs => fold(f(a.asInstanceOf[A], xs.head), xs.tail)(f)
    }

  private def fold[A](init: A, xs: List[Atom])(f: (A, Atom) => A): A =
    (init /: xs) { (a, x) => f(a, x) }

}