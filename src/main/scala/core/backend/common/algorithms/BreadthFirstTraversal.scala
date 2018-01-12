package core.backend.common.algorithms

import scala.collection.immutable.Queue

/**
  * A number of utility methods for doing pathfinding by a breadth first traversal.
  * This is used for the SQL Backend implementation, which has typically gathered all the results from the database
  */

object BreadthFirstTraversal {
  /**
    * Runs a breadth first traversal of a subgraph defined by the relation function f
    * @param node - starting node
    * @param f - step function giving all related nodes
    * @tparam Node - type of node
    * @return A stream of reachable nodes and paths to them
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


  /**
    * Run a breadth first traversal
    * @param start - Starting node
    * @param generator - a function generating the edges of the subgraph to search
    * @param end - the node to search for
    * @tparam Node - generic type parameter
    * @return An optional vector representing the first found path
    */
  def breadthFirstSearch[Node](start: Node, generator: Node => Set[Node], end: Node): Option[Vector[Node]] = {
    breadthFirstTraversal(start, generator).collectFirst { case (n, l) if n == end => l}
  }

  /**
    * Run a breadth first traversal to find all reachable nodes
    * @param start - initial set
    * @param generator - function generating the edges of teh subgraph to search
    * @tparam Node - generic type parameter
    * @return a set of the shortest path to each reachable nodes
    */

  def allPaths[Node](start: Node, generator: Node => Set[Node]): Set[Vector[Node]] = {
    breadthFirstTraversal(start, generator).collect{case (n, l) if n != start => l}.toSet
  }
}