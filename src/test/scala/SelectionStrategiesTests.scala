import NeuralNetwork.{NeuralNetwork, SelectionStrategies}
import org.scalatest.FunSuite

import scala.util.Random

class SelectionStrategiesTests extends FunSuite {
  val neuralNetworks = List(
    (NeuralNetwork(1), 0.5),
    (NeuralNetwork(2), 0.8),
    (NeuralNetwork(3), 1.7),
    (NeuralNetwork(4), 0.2)
  )
  test("SelectionStrategies.topBest") {
    assert(
      SelectionStrategies
        .topBest(
          neuralNetworks, 2, new Random(100)
        ) === List(NeuralNetwork(3), NeuralNetwork(2))
    )
  }
  test("SelectionStrategies.rank") {
    assert(
      SelectionStrategies
        .rank(
          neuralNetworks, 2, new Random(990)
        ) === List(NeuralNetwork(3), NeuralNetwork(3))
    )
  }
  test("SelectionStrategies.roulette") {
    assert(
      SelectionStrategies
        .roulette(
          neuralNetworks, 2, new Random(1109)
        ) === List(NeuralNetwork(3), NeuralNetwork(3))
    )
  }
}
