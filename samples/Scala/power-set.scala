def powerset[A](s: Set[A]) = s.foldLeft(Set(Set.empty[A])) { case (ss, el) => ss ++ ss.map(_ + el) }
