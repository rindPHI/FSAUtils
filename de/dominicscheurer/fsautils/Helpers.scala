package de.dominicscheurer.fsautils {
	object Helpers {
		def powerSet[A](s: Set[A]) =
		    s.foldLeft(Set(Set.empty[A])) {
		      (set, element) =>
		        set union (set map (_ + element))
		    }
	}
}