package core.backend.common.algorithms

import scala.collection.immutable.Queue

object Algorithms {
  /**
    * Runs a breadth first traversal of a subgraph defined by the relation function f
    * @param node - starting node
    * @param f - step function giving all related nodes
    * @tparam Node - type of node
    * @return
    */

  def breadthFirstTraversal[Node](node: Node, f: Node => Set[Node]): Stream[(Node, Vector[Node])] = {
    /*
     * Lifts the relation function into one that takes a path and returns a set of paths to unseen nodes
     */
    def fPrime(l: Vector[Node], alreadySeen: Set[Node]): Set[Vector[Node]] = l match {
      case _ :+ h =>
        f(h).diff(alreadySeen).map(l :+ _)
      case _ => Set()
    }

    /*
     * Given a queue of nodes to visit, recursively visit them and carry out a traversal
     */
    def recurse(q: Queue[Vector[Node]], alreadySeen: Set[Node]): Stream[Vector[Node]] = {
      if (q.isEmpty) {
        Stream.Empty
      } else {
        val (path, tail) = q.dequeue
        path #:: recurse(tail.enqueue(fPrime(path, alreadySeen)), alreadySeen + path.last)
      }
    }

    recurse(Queue(Vector(node)), Set(node))
      .collect {case rest :+ head => (head, rest :+ head)}
  }



  def breadthFirstSearch[Node](start: Node, generator: Node => Set[Node], end: Node): Option[Vector[Node]] = {
    breadthFirstTraversal(start, generator).collectFirst { case (n, l) if n == end => l}
  }

  def allPaths[Node](start: Node, generator: Node => Set[Node]): Set[Vector[Node]] = {
    breadthFirstTraversal(start, generator).collect{case (n, l) if n != start => println("Path = " + (n, l)); l}.toSet
  }
}