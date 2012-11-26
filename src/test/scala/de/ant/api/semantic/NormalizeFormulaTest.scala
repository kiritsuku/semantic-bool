package de.ant.api.semantic

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec

@RunWith(classOf[JUnitRunner])
class NormalizeFormulaTest extends FlatSpec {

  behavior of "function 'normalize'"

  it should "normalize simple formula" in {
    assert(show(normalize('a ∧ 'a)) === "a")
    assert(show(normalize('a ∨ 'a)) === "a")
    assert(show(normalize('a → 'b)) === "¬a ∨ b")
    assert(show(normalize('a ↔ 'b)) === "¬a¬b ∨ ab")
    assert(show(normalize('a ⊕ 'b)) === "¬ab ∨ a¬b")
    assert(show(normalize(¬(¬('a)))) === "a")
  }

  it should "normalize nested formula" in {
    assert(show(normalize(('a → 'b) → ('a ↔ 'b))) === "¬(¬a ∨ b) ∨ ¬a¬b ∨ ab")
  }
}