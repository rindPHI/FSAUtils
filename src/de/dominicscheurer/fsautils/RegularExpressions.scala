/* Copyright 2014 Dominic Scheurer
 * 
 * This file is part of FSAUtils.
 * 
 * FSAUtils is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * FSAUtils is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *  
 * You should have received a copy of the GNU General Public License
 * along with FSAUtils. If not, see <http://www.gnu.org/licenses/>.
 */

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
}