package de.ant.semantic.formula
import scalaz.syntax.id._

trait SemanticBehavior extends Formulas {

  type Leaf = Set[Formula]

  def applyRule(f: Formula): List[Leaf] = f match {
    case f if isLiteral(f) => List(Set(f))

    case ¬(¬(a)) => List(Set(a))

    case a ∧ b => List(Set(a, b))

    case ¬(a ∨ b) => List(Set(¬(a), ¬(b)))

    case ¬(a → b) => List(Set(a, ¬(b)))

    case a ∨ b => List(Set(a), Set(b))

    case ¬(a ∧ b) => List(Set(¬(a)), Set(¬(b)))

    case a → b => List(Set(¬(a)), Set(b))

    case a ↔ b => List(Set(a, b), Set(¬(a), ¬(b)))

    case ¬(a ↔ b) => List(Set(a, ¬(b)), Set(¬(a), b))
  }

  def dnf(f: Formula): Formula = {
    val (r, a) = (normalize(f), atoms(f))
    ???
  }

  def atoms(f: Formula): Set[Atom] = f match {
    case `_0` | `_1` => Set()
    case a: Atom     => Set(a)
    case ¬(a)        => atoms(a)
    case a ∧ b       => atoms(a) ++ atoms(b)
    case a ∨ b       => atoms(a) ++ atoms(b)
    case a → b       => atoms(a) ++ atoms(b)
    case a ↔ b       => atoms(a) ++ atoms(b)
    case a ⊕ b       => atoms(a) ++ atoms(b)
  }

  def normalize(f: Formula): Formula = f match {
    case Atom(_)         => f
    case ¬(¬(f))         => normalize(f)
    case ¬(f)            => ¬(normalize(f))
    case a ∧ a_ if a == a_ => a
    case a ∨ a_ if a == a_ => a

    case (a ∧ b) ∨ (a_ ∧ ¬(b_)) if a == a_ && b == b_ => a

    case a ∧ b           => normalize(a) ∧ normalize(b)
    case a ∨ b           => normalize(a) ∨ normalize(b)
    case a → b           => ¬(normalize(a)) ∨ normalize(b)
    case a ↔ b           => (normalize(a), normalize(b)) |> { case (a, b) => (¬(a) ∧ ¬(b)) ∨ (a ∧ b) }
    case a ⊕ b           => (normalize(a), normalize(b)) |> { case (a, b) => (¬(a) ∧ b) ∨ (a ∧ ¬(b)) }
  }

  def reduce(f: Formula): Formula = ???

  def axioms(f: Formula): Formula = f match {
    case a ∨ `_0`                   => a
    case a ∨ `_1`                   => _1
    case a ∧ `_1`                   => a
    case a ∧ `_0`                   => _0
    case a ∧ ¬(a_) if a == a_       => _0
    case a ∨ ¬(a_) if a == a_       => _1
    case ¬(`_0`)                    => _1
    case ¬(`_1`)                    => _0
    case a ∨ (a_ ∧ _) if a == a_    => a
    case a ∧ (a_ ∨ _) if a == a_    => a
    case a ∧ (b ∨ c)                => (a ∧ b) ∨ (a ∧ c)
    case a ∨ (b ∧ c)                => (a ∨ b) ∧ (a ∨ c)
    case (a ∧ ¬(b)) ∨ b_ if b == b_ => a ∨ b
    case (a ∨ ¬(b)) ∧ b_ if b == b_ => a ∧ b
    case f                          => f
  }

  def transform(f: Formula): Formula = f match {
    case ¬(a) ∨ ¬(b)                                     => ¬(a ∧ b)
    case ¬(a ∧ b)                                        => ¬(a) ∨ ¬(b)
    case ¬(a) ∧ ¬(b)                                     => ¬(a ∨ b)
    case ¬(a ∨ b)                                        => ¬(a) ∧ ¬(b)
    case (¬(a) ∨ ¬(b)) ↔ (a_ ∨ b_) if a == a_ && b == b_ => a ⊕ b
    case f                                               => f
  }

  def show(f: Formula): String = f match {
    case Atom(s)                           => s.name
    case `_0`                              => "0"
    case `_1`                              => "1"
    case ¬(a @ (Atom(_) | `_0` | `_1`))    => s"¬${show(a)}"
    case ¬(a)                              => s"¬(${show(a)})"

    case (a ∨ b) ∨ c                       => s"${show(a)} ∨ ${show(b)} ∨ ${show(c)}"
    case a ∨ (b ∨ c)                       => s"${show(a)} ∨ ${show(b)} ∨ ${show(c)}"
    case (a ∧ b) ∧ c                       => s"${show(a)}${show(b)}${show(c)}"
    case a ∧ (b ∧ c)                       => s"${show(a)}${show(b)}${show(c)}"

    case a ∧ b if a.isNative && b.isNative => s"${show(a)}${show(b)}"
    case a ∧ b if a.isNative               => s"${show(a)}(${show(b)})"
    case a ∧ b if b.isNative               => s"(${show(a)})${show(b)}"
    case a ∧ b                             => s"(${show(a)})(${show(b)})"

    case a ∨ b if a.isNative && b.isNative => s"${show(a)} ∨ ${show(b)}"
    case a ∨ b if a.isNative               => s"${show(a)} ∨ (${show(b)})"
    case a ∨ b if b.isNative               => s"(${show(a)}) ∨ ${show(b)}"
    case a ∨ b                             => s"(${show(a)}) ∨ (${show(b)})"

    case a → b if a.isNative && b.isNative => s"${show(a)} → ${show(b)}"
    case a → b if a.isNative               => s"${show(a)} → (${show(b)})"
    case a → b if b.isNative               => s"(${show(a)}) → ${show(b)}"
    case a → b                             => s"(${show(a)}) → (${show(b)})"

    case a ↔ b if a.isNative && b.isNative => s"${show(a)} ↔ ${show(b)}"
    case a ↔ b if a.isNative               => s"${show(a)} ↔ (${show(b)})"
    case a ↔ b if b.isNative               => s"(${show(a)}) ↔ ${show(b)}"
    case a ↔ b                             => s"(${show(a)}) ↔ (${show(b)})"

    case a ⊕ b if a.isNative && b.isNative => s"${show(a)} ⊕ ${show(b)}"
    case a ⊕ b if a.isNative               => s"${show(a)} ⊕ (${show(b)})"
    case a ⊕ b if b.isNative               => s"(${show(a)}) ⊕ ${show(b)}"
    case a ⊕ b                             => s"(${show(a)}) ⊕ (${show(b)})"
  }

  def isLiteral(f: Formula): Boolean = f match {
    case Atom(_) => true
    case ¬(Atom(_)) => true
    case _ => false
  }

  def semanticTableau(f: Formula): List[Leaf] = {

    def combine(rec: List[Leaf], f: Formula): List[Leaf] =
      for (a <- applyRule(f); b <- rec) yield (a ++ b)

    def openLeaf(leaf: Leaf): List[Leaf] =
      if (leaf forall isLiteral)
        List(leaf)
      else
        leaf.foldLeft(List(Set.empty: Leaf))(combine) flatMap (openLeaf)

    openLeaf(Set(f))
  }

  def isClosedLeaf(f: Leaf): Boolean = {
    if (f.isEmpty) false
    else {
      (f.head match {
        case Atom(_) => f.tail.exists(_ == ¬(f.head))
        case ¬(Atom(a)) => f.tail.exists(_ == Atom(a))
        case _ => false
      }) || isClosedLeaf(f.tail)
    }
  }

  def isOpenLeaf(f: Leaf) = !isClosedLeaf(f)

  def isValid(f: Formula): Boolean = semanticTableau(¬(f)) forall isClosedLeaf

  def isSatisfiable(f: Formula): Boolean = semanticTableau(f) exists isOpenLeaf
}