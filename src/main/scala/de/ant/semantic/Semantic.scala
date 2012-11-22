package de.ant.semantic

import de.ant.S._

import de.ant.lib._

object Semantic extends App {

  val f1 = (¬('r) ∧ 'q) ∧ (¬('p ∧ 'q) ↔ (¬('p) ∨ ¬('q)))
  println(isSatisfiable(f1))
  println(applyRule(f1))
  println("show: "+show(f1))
  println("reduce: "+show(reduce(f1)))

  println(">>"+((('a ∨ ¬('a)) ∧ _1) ∨ _1 |> axioms |> show))

  println(('c ∧ ('b ⊕ 'a)) ∨ (¬('c) ∧ ('b ∨ 'a)) |> reduce |> show)
  println((¬('b) ∨ ¬('a)) ↔ ('b ∨ 'a) |> reduce |> show)

  println('c → ((¬('b) ∧ 'a) ⊕ 'c) |> reduce |> show)
}