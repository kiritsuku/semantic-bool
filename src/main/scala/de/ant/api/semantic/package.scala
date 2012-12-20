package de.ant.api

import scala.language.implicitConversions

import de.ant.semantic.formula.{Formulas, SemanticBehavior}
import de.ant.semantic.parser.FormulaParsers

package object semantic
    extends Formulas
    with SemanticBehavior
    with FormulaParsers {

  implicit def symbolToAtom(sym: Symbol): Atom = Atom(sym)
}