package de.ant.api.semantic.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import de.ant.semantic.parser.FormulaParsers
import de.ant.semantic.formula.Formulas
import de.ant.semantic.formula.SemanticBehavior

@RunWith(classOf[JUnitRunner])
class FormulaParsersTest extends FlatSpec with FormulaParsers with SemanticBehavior {

  behavior of "formula parser"

  it should "parse atom, 1, 0, ¬" in {
    assert(show(formula("a")) === "a")
    assert(show(formula("1")) === "1")
    assert(show(formula("0")) === "0")
    assert(show(formula("¬a")) === "¬a")
  }

  it should "parse ∧, ∨, →, ↔, ⊕" in {
    assert(show(formula("ab")) === "ab")
    assert(show(formula("a ∧ b")) === "ab")
    assert(show(formula("a ∨ b")) === "a ∨ b")
    assert(show(formula("a → b")) === "a → b")
    assert(show(formula("a ↔ b")) === "a ↔ b")
    assert(show(formula("a ⊕ b")) === "a ⊕ b")
  }

  it should "parse + as ∧, * as ∨" in {
    assert(show(formula("a * b")) === "ab")
    assert(show(formula("a + b")) === "a ∨ b")
  }

  def formula(in: String): Formula =
    parseFormula(in).getOrElse(sys.error("omg"))
}