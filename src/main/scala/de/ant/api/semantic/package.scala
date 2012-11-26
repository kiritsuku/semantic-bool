package de.ant.api

import scala.language.implicitConversions
import de.ant.semantic.formula.Formulas
import de.ant.semantic.formula.SemanticBehavior

package object semantic extends Formulas with SemanticBehavior {
  implicit def symbolToAtom(sym: Symbol): Atom = Atom(sym)
}