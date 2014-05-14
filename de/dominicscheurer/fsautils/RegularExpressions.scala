package de.dominicscheurer.fsautils

import Types._

object RegularExpressions {
  sealed abstract class RE {
     def *(): RE = Star(this)
     def +(rhs: RE): RE = Or(this, rhs)
     def &(rhs: RE): RE = Concat(this, rhs)
  }
  
  case class L(l: Letter) extends RE
  case class Empty() extends RE
  case class Star(re: RE) extends RE
  case class Or(lhs: RE, rhs: RE) extends RE
  case class Concat(lhs: RE, rhs: RE) extends RE
  
//  def *(re: RE): RE = Star(re)
//  def +(lhs: RE, rhs: RE): RE = Or(lhs, rhs)
//  def &(lhs: RE, rhs: RE): RE = Concat(lhs, rhs)
}