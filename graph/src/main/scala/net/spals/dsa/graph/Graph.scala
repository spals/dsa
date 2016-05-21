package net.spals.dsa.graph

/**
  * Functional implementation of a weighted graph.
  *
  * @author spags
  * @author tkral
  */
case class Edge[T, W](v1: Vertex[T], v2: Vertex[T], weight: W) {  }

case class Vertex[T](label: T) {  }

object WeightedGraph {

  def empty[T, W]: WeightedGraph[T, W] = WeightedGraph(Set.empty[Vertex[T]], (v1, v2) => Option.empty[W])
}

case class WeightedGraph[T, W](vertices: Set[Vertex[T]], edgeFunc: (Vertex[T], Vertex[T]) => Option[W]) {

  def addEdge(v1: Vertex[T], v2: Vertex[T], weight: W): WeightedGraph[T, W] = {
    val addEdgeFunc: (Vertex[T], Vertex[T]) => Option[W] =
      (vx, vy) => (vx, vy) match {
        case (vx, vy) if (vx == v1 && vy == v2) || (vx == v2 && vy == v1) => Option(weight)
        case _ => edgeFunc.apply(vx, vy)
      }
    WeightedGraph(vertices ++ Set(v1, v2), addEdgeFunc)
  }

  def addVertex(v: Vertex[T]): WeightedGraph[T, W] = {
    WeightedGraph(vertices + v, edgeFunc)
  }

  def getEdge(v1: Vertex[T], v2: Vertex[T]): Option[Edge[T, W]] = {
    edgeFunc.apply(v1, v2).map(Edge(v1, v2, _))
  }

  def getEdges(v1: Vertex[T]): Set[Edge[T, W]] = {
    vertices.map(v2 => (v2, edgeFunc.apply(v1, v2) /*weight*/))
      .filter(_._2.isDefined)
      .map(tuple => new Edge[T, W](v1, tuple._1, tuple._2.get))
  }

  def getNeighbors(v1: Vertex[T]): Set[Vertex[T]] = {
    vertices.map(v2 => (v2, edgeFunc.apply(v1, v2) /*weight*/))
      .filter(_._2.isDefined)
      .map(_._1)
  }

  def hasEdge(v1: Vertex[T], v2: Vertex[T]): Boolean = {
    edgeFunc.apply(v1, v2).isDefined
  }

  def removeEdge(v1: Vertex[T], v2: Vertex[T]): WeightedGraph[T, W] = {
    val removeEdgeFunc: (Vertex[T], Vertex[T]) => Option[W] =
      (vx, vy) => (vx, vy) match {
        case (vx, vy) if (vx == v1 && vy == v2) || (vx == v2 && vy == v1) => None
        case _ => edgeFunc.apply(vx, vy)
      }

    WeightedGraph(vertices, removeEdgeFunc)
  }

  def removeVertex(v: Vertex[T]): WeightedGraph[T, W] = {
    val removeVertexFunc: (Vertex[T], Vertex[T]) => Option[W] =
      (vx, vy) => (vx, vy) match {
        case (vx, vy) if vx == v || vy == v => None
        case _ => edgeFunc.apply(vx, vy)
      }

    WeightedGraph(vertices - v, removeVertexFunc)
  }
}
