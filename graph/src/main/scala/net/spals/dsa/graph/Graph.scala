package net.spals.dsa.graph

/**
  * Functional implementation of a weighted graph.
  *
  * @author spags
  */
class Graph[T, W] {
  // set of all vertices
  var vertices = Set[Vertex[T]]()
  // an edge function given two vertices does there an exist some weight?
  var func: (Vertex[T], Vertex[T]) => Option[W] = (v1, v2) => None

  def addVertex(v: Vertex[T]): Unit = {
    vertices += v
  }

  def removeVertex(v: Vertex[T]): Unit = {
    vertices -= v
    val temp = func
    func = (x: Vertex[T], y: Vertex[T]) =>
      if (x == v || y == v)
        None
      else
        temp(x, y)
  }

  def hasEdge(v1: Vertex[T], v2: Vertex[T]): Boolean = {
    func.apply(v1, v2).isDefined
  }

  def addEdge(v1: Vertex[T], v2: Vertex[T], w: W): Unit = {
    addVertex(v1)
    addVertex(v2)
    val temp = func
    func = (x: Vertex[T], y: Vertex[T]) =>
      if ((x == v1 && y == v2) || (x == v2 && y == v1))
        Option(w)
      else
        temp(x, y)
  }

  def getEdge(v1: Vertex[T], v2: Vertex[T]): Option[W] = {
    func.apply(v1, v2)
  }

  def getEdges(v1: Vertex[T]): Set[Edge[T, W]] = {
    var edges = Set[Edge[T, W]]()
    vertices.foreach(
      v2 => func.apply(v1, v2).foreach(w => edges += new Edge(v1, v2, w))
    )
    edges
  }

  def getNeighbors(v1: Vertex[T]): Set[Vertex[T]] = {
    var neighbors = Set[Vertex[T]]()
    vertices.foreach(
      v2 => func.apply(v1, v2).foreach(w => neighbors += v2)
    )
    neighbors
  }

  def getVertices: Set[Vertex[T]] = {
    vertices
  }
}

class Edge[T, W](v1: Vertex[T], v2: Vertex[T], w: W) {
}

class Vertex[T](v: T) {
}
