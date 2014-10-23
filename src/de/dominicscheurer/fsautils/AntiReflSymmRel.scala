package de.dominicscheurer.fsautils

import Types._

class AntiReflSymmRel[A](val values: Set[(A,A)]) {
  
	def +(a: A, b: A) = {
	  require(a != b)
	  
	  new AntiReflSymmRel(values + (a -> b) + (b -> a))
	}
	
	def inRel(a: A, b: A): Boolean =
	  values.contains(a -> b)
	  
	def !=(other: AntiReflSymmRel[A]) =
	  !values.equals(other.values)
	  
}

object EmptyRel extends AntiReflSymmRel(Set(): Set[(State,State)])