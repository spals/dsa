package net.spals.dsa.graph

/**
  * Functional implementation of a weighted graph.
  *
  * @author spags
  * @author tkral
  */
case class Edge[T, W](v1: Vertex[T], v2: Vertex[T], w: W) {  }

case class Vertex[T](v: T) {  }

case class WeightedGraph[T, W](vertices: Set[Vertex[T]], edgeFunc: (Vertex[T], Vertex[T]) => Option[W]) {

  def addEdge(v1: Vertex[T], v2: Vertex[T], w: W): WeightedGraph[T, W] = {
    val addEdgeFunc: (Vertex[T], Vertex[T]) => Option[W] =
      (vx, vy) => (vx, vy) match {
        case (vx, vy) if (vx == v1 && vy == v2) || (vx == v2 && vy == v1) => Option(w)
        case _ => edgeFunc.apply(vx, vy)
      }
    WeightedGraph(vertices ++ Set(v1, v2), addEdgeFunc)
  }

  def addVertex(v: Vertex[T]): WeightedGraph[T, W] = {
    WeightedGraph(vertices + v, edgeFunc)
  }

  def getEdge(v1: Vertex[T], v2: Vertex[T]): Option[W] = {
    edgeFunc.apply(v1, v2)
  }

  def getEdges(v1: Vertex[T]): Set[Edge[T, W]] = {
    vertices.map(v2 => (v2, edgeFunc.apply(v1, v2)))
      .filter(_._2.isDefined)
      .map(tuple => new Edge[T, W](v1, tuple._1, tuple._2.get))
  }

  def getNeighbors(v1: Vertex[T]): Set[Vertex[T]] = {
    vertices.map(v2 => (v2, edgeFunc.apply(v1, v2)))
      .filter(_._2.isDefined)
      .map(_._1)
  }

  def hasEdge(v1: Vertex[T], v2: Vertex[T]): Boolean = {
    edgeFunc.apply(v1, v2).isDefined
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
