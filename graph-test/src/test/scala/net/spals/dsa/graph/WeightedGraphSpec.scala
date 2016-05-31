package net.spals.dsa.graph

import org.scalatest.{FunSpec, GivenWhenThen, Matchers}

/**
  * BDD tests for [[WeightedGraph]]
  *
  * @author tkral
  */
class WeightedGraphSpec extends FunSpec with GivenWhenThen with Matchers {

  describe("A weighted graph") {
    it("should have an empty state without vertices") {
      Given("an empty graph")
      val emptyGraph = WeightedGraph.empty

      Then("no vertices should be present")
      emptyGraph.vertices.size should be(0)
    }
  }

  it("should allow for vertices with edges") {
    Given("a graph with only two vertices")
    val wGraph = WeightedGraph.empty[String, Double].addVertex(Vertex("a")).addVertex(Vertex("b"))

    When("an edge with weight 1.0 is added between them")
    val newWGraph = wGraph.addEdge(Vertex("a"), Vertex("b"), 1.0d)

    Then("an edge should exist between them")
    newWGraph.hasEdge(Vertex("a"), Vertex("b")) should be(true)
    val edge = newWGraph.getEdge(Vertex("a"), Vertex("b"))
    edge should not be None

    And("the edge has a weight of 1.0")
    edge.get.weight should be(1.0d)

    And("both vertices should still exist")
    newWGraph.vertices.size should be(2)
  }

  it("should allow for vertices without edges") {
    Given("a graph with two vertices and an edge")
    val wGraph = WeightedGraph.empty.addEdge(Vertex("a"), Vertex("b"), 1.0d)

    When("the edge is removed")
    val newWGraph = wGraph.removeEdge(Vertex("a"), Vertex("b"))

    Then("no edge exists between them")
    newWGraph.hasEdge(Vertex("a"), Vertex("b")) should be(false)

    And("both vertices should still exist")
    newWGraph.vertices.size should be(2)
  }

  it("should allow us to override edge weights") {
    val wGraph = WeightedGraph.empty
      .addEdge(Vertex("a"), Vertex("b"), 1.0d)

    When("the edge is overridden")
    val newGraph = wGraph.addEdge(Vertex("a"), Vertex("b"), 2.0d)

    Then("the edge still exists between them")
    newGraph.hasEdge(Vertex("a"), Vertex("b")) should be(true)

    And("number of vertices is still two")
    newGraph.vertices.size should be(2)

    And("weight is now two")
    newGraph.getEdge(Vertex("a"), Vertex("b")).get.weight should be(2.0d)
  }

  it("dijkstras") {
    Given("a fully connected graph with three vertices")
    val graph = WeightedGraph.empty
      .addEdge(Vertex("a"), Vertex("b"), 1.0d)
      .addEdge(Vertex("b"), Vertex("c"), 1.0d)
      .addEdge(Vertex("a"), Vertex("c"), 5.0d)

    When("there is a shortest path from a to c through b")
    val (dist, prev) = things.dijkstra(graph, Vertex("a"))

    Then("here are the shortest paths")
    dist.get(Vertex("a")).get.get should be(0.0d)
    dist.get(Vertex("b")).get.get should be(1.0d)
    dist.get(Vertex("c")).get.get should be(2.0d)

    And("here are the predecessors")
    prev.get(Vertex("a")).isDefined should be(false)
    prev.get(Vertex("b")).get should be(Vertex("a"))
    prev.get(Vertex("c")).get should be(Vertex("b"))
  }
}
