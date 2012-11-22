package de.ant

import scala.language.implicitConversions
import de.ant.semantic.formula.Formulas
import de.ant.semantic.SemanticBehavior

package object semantic {}

//package object semantic extends Formulas with SemanticBehavior {
//
//  implicit def symbolToAtom(sym: Symbol): Atom = Atom(sym)
//}

object S extends Formulas with SemanticBehavior {
  implicit def symbolToAtom(sym: Symbol): Atom = Atom(sym)
}