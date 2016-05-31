package net.spals.dsa.graph

import scala.collection.mutable

/**
  * Functional implementation of a weighted graph.
  *
  * @author spags
  * @author tkral
  */
trait Graph[T, W] {

  def addVertex(v: Vertex[T]): Graph[T, W]

  def getEdge(v1: Vertex[T], v2: Vertex[T]): Option[Edge[T, W]]

  def getEdges(v: Vertex[T]): Set[Edge[T, W]]

  def getNeighbors(v: Vertex[T]): Set[Vertex[T]]

  def getVertices: Set[Vertex[T]]

  def getWeight(v1: Vertex[T], v2: Vertex[T]): Option[W]

  def hasEdge(v1: Vertex[T], v2: Vertex[T]): Boolean

  def removeEdge(v1: Vertex[T], v2: Vertex[T]): Graph[T, W]

  def removeVertex(v: Vertex[T]): Graph[T, W]

  //  def sumEdges[N >: W](implicit num: Numeric[N]): W
}

case class Edge[T, W](v1: Vertex[T], v2: Vertex[T], weight: W) {}

case class Vertex[T](label: T) {}

object WeightedGraph {

  def empty[T, W]: WeightedGraph[T, W] = WeightedGraph(Set.empty[Vertex[T]], (v1, v2) => Option.empty[W])
}

case class WeightedGraph[T, W](vertices: Set[Vertex[T]], edgeFunc: (Vertex[T], Vertex[T]) => Option[W])
  extends Graph[T, W] {

  def addEdge(v1: Vertex[T], v2: Vertex[T], weight: W): WeightedGraph[T, W] = {
    val addEdgeFunc: (Vertex[T], Vertex[T]) => Option[W] =
      (vx, vy) => (vx, vy) match {
        case (`vx`, `vy`) if (vx == v1 && vy == v2) || (vx == v2 && vy == v1) => Option(weight)
        case _ => edgeFunc.apply(vx, vy)
      }
    WeightedGraph(vertices ++ Set(v1, v2), addEdgeFunc)
  }

  override def addVertex(v: Vertex[T]): WeightedGraph[T, W] =
    WeightedGraph(vertices + v, edgeFunc)

  override def getEdges(v: Vertex[T]): Set[Edge[T, W]] =
    vertices.map(v2 => (v2, edgeFunc.apply(v, v2) /*weight*/ ))
      .filter(_._2.isDefined)
      .map(tuple => new Edge[T, W](v, tuple._1, tuple._2.get))

  override def getNeighbors(v: Vertex[T]): Set[Vertex[T]] =
    vertices.map(v2 => (v2, edgeFunc.apply(v, v2) /*weight*/ ))
      .filter(_._2.isDefined)
      .map(_._1)

  override def getVertices = vertices

  override def getWeight(v1: Vertex[T], v2: Vertex[T]): Option[W] =
    getEdge(v1, v2).map(_.weight)

  override def getEdge(v1: Vertex[T], v2: Vertex[T]): Option[Edge[T, W]] =
    edgeFunc.apply(v1, v2).map(Edge(v1, v2, _))

  override def hasEdge(v1: Vertex[T], v2: Vertex[T]): Boolean = {
    edgeFunc.apply(v1, v2).isDefined
  }

  override def removeEdge(v1: Vertex[T], v2: Vertex[T]): WeightedGraph[T, W] = {
    val removeEdgeFunc: (Vertex[T], Vertex[T]) => Option[W] =
      (vx, vy) => (vx, vy) match {
        case (`vx`, `vy`) if (vx == v1 && vy == v2) || (vx == v2 && vy == v1) => None
        case _ => edgeFunc.apply(vx, vy)
      }

    WeightedGraph(vertices, removeEdgeFunc)
  }

  override def removeVertex(v: Vertex[T]): WeightedGraph[T, W] = {
    val removeVertexFunc: (Vertex[T], Vertex[T]) => Option[W] =
      (vx, vy) => (vx, vy) match {
        case (`vx`, `vy`) if vx == v || vy == v => None
        case _ => edgeFunc.apply(vx, vy)
      }

    WeightedGraph(vertices - v, removeVertexFunc)
  }

  //  override def sumEdges[N >: W](implicit num: Numeric[N]): N = {
  //    val x: N = vertices.map(getEdges(_).map(_.weight).sum / ).sum
  //  }
}

object SimpleGraph {

  def empty[T]: SimpleGraph[T] = SimpleGraph(WeightedGraph.empty[T, Int])
}

/**
  * Defined as a weighted graph with all edge weights as a unit, i.e. the integer value of 1.
  */
case class SimpleGraph[T](delegate: WeightedGraph[T, Int]) extends Graph[T, Int] {

  def addEdge(v1: Vertex[T], v2: Vertex[T]): SimpleGraph[T] = SimpleGraph(delegate.addEdge(v1, v2, 1))

  override def addVertex(v: Vertex[T]): SimpleGraph[T] = SimpleGraph(delegate.addVertex(v))

  override def getEdge(v1: Vertex[T], v2: Vertex[T]): Option[Edge[T, Int]] = delegate.getEdge(v1, v2)

  override def getEdges(v: Vertex[T]): Set[Edge[T, Int]] = delegate.getEdges(v)

  override def getNeighbors(v: Vertex[T]): Set[Vertex[T]] = delegate.getNeighbors(v)

  override def getVertices = delegate.getVertices

  override def getWeight(v1: Vertex[T], v2: Vertex[T]) = delegate.getWeight(v1, v2)

  override def hasEdge(v1: Vertex[T], v2: Vertex[T]): Boolean = delegate.hasEdge(v1, v2)

  override def removeEdge(v1: Vertex[T], v2: Vertex[T]): SimpleGraph[T] = SimpleGraph(delegate.removeEdge(v1, v2))

  override def removeVertex(v: Vertex[T]): SimpleGraph[T] = SimpleGraph(delegate.removeVertex(v))

  //  override def sumEdges[]: Int = delegate.sumEdges
}

//  override def sumEdges[N >: W](implicit num: Numeric[N]): N = {


object things {

  def dijkstra[T, W](graph: Graph[T, W], source: Vertex[T])(implicit n: Numeric[W]): (mutable.HashMap[Vertex[T], Option[W]], mutable.HashMap[Vertex[T], Vertex[T]]) = {
    val vertexes = new mutable.HashSet[Vertex[T]]
    val dist = new mutable.HashMap[Vertex[T], Option[W]]
    val prev = new mutable.HashMap[Vertex[T], Vertex[T]]

    graph.getVertices.foreach(
      v => {
        dist.put(v, Option.empty)
        vertexes.add(v)
      }
    )
    dist.put(source, Option(n.zero))

    while (vertexes.nonEmpty) {
      // this is suppose to find the minimum distance thus far, a more efficient implementation would be using a
      // priority queue
      val (minV, minW) = vertexes.foldLeft((vertexes.head, dist(vertexes.head))) {
        case ((v1, Some(w1)), v2) => dist(v2) match {
          case Some(w2) => if (n.compare(w1, w2) > 0) {
            (v2, Option(w2))
          } else {
            (v1, Option(w1))
          }
          case None => (v1, Option(w1))
        }
        case ((v1, None), v2) => (v2, dist(v2))
      }
      vertexes.remove(minV)
      graph.getNeighbors(minV)
        .intersect(vertexes)
        .foreach(
          v => {
            minW match {
              case Some(w1) =>
                val distance = n.plus(w1, graph.getEdge(minV, v).get.weight)
                dist(v) match {
                  case Some(w2) =>
                    if (n.compare(distance, w2) < 0) {
                      dist.put(v, Option(distance))
                      prev.put(v, minV)
                    }
                  case None =>
                    dist.put(v, Option(distance))
                    prev.put(v, minV)
                }
              case None =>
                throw new IllegalStateException("this is not possible as min vertex weight has to be defined here.")
            }
          }
        )
    }
    (dist, prev)
  }
}