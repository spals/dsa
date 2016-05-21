package net.spals.dsa.graph

import org.scalatest.{FunSpec, Matchers}

/**
  * BDD tests for [[WeightedGraph]]
  *
  * @author tkral
  */
class WeightedGraphSpec extends FunSpec with Matchers {

  describe("A weighted graph") {
    it("should have an empty state") {
      val emptyGraph = WeightedGraph.empty
      emptyGraph.vertices.size should be (0)
    }
  }
}
