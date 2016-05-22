package net.spals.dsa.graph

import org.scalatest.{GivenWhenThen, FunSpec, Matchers}

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
      emptyGraph.vertices.size should be (0)
    }
  }

  it("should allow for vertices with edges") {
    Given("a graph with only two vertices")
    val wGraph = WeightedGraph.empty[String, Double].addVertex(Vertex("a")).addVertex(Vertex("b"))

    When("an edge with weight 1.0 is added between them")
    val newWGraph = wGraph.addEdge(Vertex("a"), Vertex("b"), 1.0d)

    Then("an edge should exist between them")
    newWGraph.hasEdge(Vertex("a"), Vertex("b")) should be (true)
    val edge = newWGraph.getEdge(Vertex("a"), Vertex("b"))
    edge should not be (None)

    And("the edge has a weight of 1.0")
    edge.get.weight should be (1.0d)

    And("both vertices should still exist")
    newWGraph.vertices.size should be (2)
  }

  it("should allow for vertices without edges") {
    Given("a graph with two vertices and an edge")
    val wGraph = WeightedGraph.empty.addEdge(Vertex("a"), Vertex("b"), 1.0d)

    When("the edge is removed")
    val newWGraph = wGraph.removeEdge(Vertex("a"), Vertex("b"))

    Then("no edge exists between them")
    newWGraph.hasEdge(Vertex("a"), Vertex("b")) should be (false)

    And("both vertices should still exist")
    newWGraph.vertices.size should be (2)
  }
}
