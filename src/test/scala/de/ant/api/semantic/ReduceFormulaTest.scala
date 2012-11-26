package de.ant.api.semantic

import org.scalatest.FlatSpec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ReduceFormulaTest extends FlatSpec {

  behavior of "function 'reduce'"

  it should "reduce simple formula" in {
    assert(show(reduce(('a ∧ 'b) ∨ ('a ∧ ¬('b)))) === "a")
    assert(show(reduce(('a ∧ ¬('b)) ∨ 'b)) === "a ∨ b")
    assert(show(reduce(('a ∨ ¬('b)) ∧ 'b)) === "ab")
    assert(show(reduce((¬('a) ∧ 'b ∧ 'c) ∨ ¬('a))) === "¬a")
  }
}