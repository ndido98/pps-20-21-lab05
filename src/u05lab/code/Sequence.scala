package u05lab.code

import scala.collection.immutable.List

object Sequence {
    def sequence[A](a: List[Option[A]]): Option[List[A]] =
        a.foldLeft(Some(List.empty[A]): Option[List[A]])((acc, elem) => (acc, elem) match {
            case (Some(list), Some(value)) => Some(list :+ value)
            case _ => None
        })
}
