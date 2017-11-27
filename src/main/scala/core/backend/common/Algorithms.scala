package core.backend.common

import scala.collection.immutable.Queue
import core.utils._

object Algorithms {
  def breadthFirstTraversal[Node](node: Node, f: Node => Set[Node]): Stream[(Node, List[Node])] = {
    def fPrime(l: List[Node], alreadySeen: Set[Node]): Set[List[Node]] = l match {
      case h :: _ =>
        f(h).diff(alreadySeen).map(_ :: l)
      case Nil => Set()
    }

    def recurse(q: Queue[List[Node]], alreadySeen: Set[Node]): Stream[List[Node]] = {
      if (q.isEmpty) {
        Stream.Empty
      } else {
        val (path, tail) = q.dequeue
        path #:: recurse(tail ++ fPrime(path, alreadySeen), alreadySeen + path.head)
      }
    }

    (List(node) #:: recurse(Queue.empty ++ fPrime(List(node), Set(node)), Set(node)))
      .collect {case h :: t => (h, h :: t)}
  }



  def breadthFirstSearch[Node](start: Node, generator: Node => Set[Node], end: Node): Option[List[Node]] = {
    breadthFirstTraversal(start, generator).collectFirst { case (n, l) if n == end => l}
  }

  def allPaths[Node](start: Node, generator: Node => Set[Node]): Set[List[Node]] = {
    breadthFirstTraversal(start, generator).map{case (_, l: List[Node]) => l}.toSet
  }
}