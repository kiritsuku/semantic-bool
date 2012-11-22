package de.ant

package object lib {

  // forward pipe operator
  implicit class PipedObject[A](val $value: A) extends AnyVal {
    def |>[B](f: A => B): B = f($value)
  }
}