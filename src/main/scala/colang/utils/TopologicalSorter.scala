package colang.utils

import colang.utils.TopologicalSorter.{LoopFound, SortResult, Sorted}

import scala.collection.mutable

/**
  * A component that performs a topological sort of some set of objects (nodes).
  * A topological sort is usually used for ordering some objects that depend on each other so that all dependencies
  * of an object come before it.
  * @param edges a function that calculates the outgoing edges from a given node
  * @tparam T object (node) type
  */
class TopologicalSorter[T](edges: T => Iterable[T]) {

  private object State extends Enumeration {
    type State = Value
    val NotVisited, Pending, Visited = Value
  }
  import State._

  private def findLoop(objects: Seq[T]): Option[List[T]] = {
    val vertexStates = mutable.Map(objects map { (_, NotVisited) } :_*)

    def dfs(vertex: T): Option[List[T]] = {
      vertexStates(vertex) = Pending

      val loop = edges(vertex).foldLeft[Option[List[T]]] (None) { (foundLoop, adjacent) =>
        foundLoop.orElse(vertexStates(adjacent) match {
          case NotVisited => dfs(adjacent) map { vertex :: _ }
          case Pending => Some(vertex :: adjacent :: Nil)
          case Visited => None
        })
      }

      vertexStates(vertex) = Visited
      loop
    }

    val loopWithPrefix = objects.foldLeft[Option[List[T]]] (None) { (foundLoop, vertex) =>
      foundLoop.orElse(if (vertexStates(vertex) == NotVisited) dfs(vertex) else None)
    }

    loopWithPrefix map { lwp =>
      val loopStartIndex = lwp.indexOf(lwp.last)
      lwp drop loopStartIndex
    }
  }

  /**
    * Sorts the given objects.
    * @param objects objects to sort
    * @return either Sorted(objects) or LoopFound(loop), depending on whether a circular dependency chain was found
    */
  def sort(objects: Seq[T]): SortResult[T] = {
    findLoop(objects) match {
      case Some(loop) => LoopFound(loop)
      case None =>
        val vertexStates = mutable.Map(objects map { (_, NotVisited) } :_*)

        def dfs(vertex: T): Vector[T] = {
          vertexStates(vertex) = Visited

          edges(vertex).foldLeft(Vector.empty[T]) { (result, adjacent) =>
            if (vertexStates(adjacent) == NotVisited) {
              result ++ dfs(adjacent)
            } else {
              result
            }
          } :+ vertex
        }

        val order = objects.foldLeft(Vector.empty[T]) { (result, vertex) =>
          if (vertexStates(vertex) == NotVisited) {
            result ++ dfs(vertex)
          } else {
            result
          }
        }

        Sorted(order)
    }
  }
}

object TopologicalSorter {
  sealed trait SortResult[T]

  case class Sorted[T](objects: Seq[T]) extends SortResult[T]
  case class LoopFound[T](loop: Seq[T]) extends SortResult[T]
}
