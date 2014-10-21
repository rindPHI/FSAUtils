package de.dominicscheurer.fsautils {
	object Helpers {
		def powerSet[A](s: Set[A]) =
		    s.foldLeft(Set(Set.empty[A])) {
		      (set, element) =>
		        set union (set map (_ + element))
		    }
		
		def optJoin[A](a: Option[Set[A]], b: Option[Set[A]]): Option[Set[A]] =
			a match {
			  case Some(s1) => b match {
			    case Some(s2) => Some(s1 ++ s2)
			    case None => Some(s1)
			  }
			  case None => b match {
			    case Some(s) => Some(s)
			    case None => None
			  }
			}
		
		def optJoin[A](a: Option[Set[A]]*) : Option[Set[A]] =
		  a.foldLeft(None: Option[Set[A]])((acc,elem) => optJoin(acc,elem))
	}
}