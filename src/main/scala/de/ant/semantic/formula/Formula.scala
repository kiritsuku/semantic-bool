package de.ant.semantic.formula

trait Formulas {
  sealed abstract class Formula {
    final def ∧(a: Formula) = new ∧(this, a)
    final def and(a: Formula) = ∧(a)

    final def ∨(a: Formula) = new ∨(this, a)
    final def or(a: Formula) = ∨(a)

    final def →(a: Formula) = new →(this, a)
    final def implies(a: Formula) = →(a)

    final def ↔(a: Formula) = new ↔(this, a)
    final def equivalent(a: Formula) = ↔(a)

    final def ⊕(a: Formula) = new ⊕(this, a)
    final def antivalent(a: Formula) = ⊕(a)

    def isNative: Boolean
  }

  case class Atom(symbol: Symbol) extends Formula {
    def isNative = true
  }

  case object _0 extends Formula {
    def isNative = true
  }
  type _0 = _0.type

  case object _1 extends Formula {
    def isNative = true
  }
  type _1 = _1.type

  object Negation {
    def apply(a: Formula): ¬ = ¬(a)
  }
  case class ¬(a: Formula) extends Formula {
    def isNative = true
  }

  object Conjunction {
    def apply(a: Formula, b: Formula): ∧ = ∧(a, b)
  }
  case class ∧(a: Formula, b: Formula) extends Formula {
    def isNative = true
  }

  object Disjunction {
    def apply(a: Formula, b: Formula): ∨ = ∨(a, b)
  }
  case class ∨(a: Formula, b: Formula) extends Formula {
    def isNative = false
  }

  object Implication {
    def apply(a: Formula, b: Formula): → = →(a, b)
  }
  case class →(a: Formula, b: Formula) extends Formula {
    def isNative = false
  }

  object Equivalence {
    def apply(a: Formula, b: Formula): ↔ = ↔(a, b)
  }
  case class ↔(a: Formula, b: Formula) extends Formula {
    def isNative = false
  }

  object Antivalence {
    def apply(a: Formula, b: Formula): ⊕ = ⊕(a, b)
  }
  case class ⊕(a: Formula, b: Formula) extends Formula {
    def isNative = false
  }

}