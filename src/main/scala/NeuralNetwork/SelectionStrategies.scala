package NeuralNetwork

import scala.util.Random

case object SelectionStrategies {
  /**
   * Sorts the neural networks based on their fitness in descending order and takes the top best
   * @param neuralNetworks List of neural networks paired with their fitness scores
   * @param selectionSize The amount of neural networks to take
   * @param rand pseudo-random number generator
   * @return A list of neural networks
   */
  def topBest(neuralNetworks: List[(NeuralNetwork, Double)], selectionSize: Int, rand: Random): List[NeuralNetwork] = {
    neuralNetworks
      .sortBy(x => x._2)(Ordering[Double].reverse)
      .take(selectionSize)
      .map(x => x._1)
  }

  def rank(neuralNetworks: List[(NeuralNetwork, Double)], selectionSize: Int, rand: Random): List[NeuralNetwork] = {
    probabilityBasedSelection(rankNormalization(neuralNetworks), selectionSize, rand, List())
  }

  def roulette(neuralNetworks: List[(NeuralNetwork, Double)], selectionSize: Int, rand: Random): List[NeuralNetwork] = {
    probabilityBasedSelection(normalizeFitnesses(neuralNetworks), selectionSize, rand, List())
  }

  @scala.annotation.tailrec
  private def probabilityBasedSelection
  (neuralNetworks: List[(NeuralNetwork, Double)],
   selectionSize: Int,
   rand: Random,
   accu: List[NeuralNetwork]): List[NeuralNetwork] = {
    if(selectionSize == 0) {
      accu
    } else {
      val p = rand.nextDouble()
      probabilityBasedSelection(neuralNetworks, selectionSize-1, rand, accu.prepended(neuralNetworks.find(x => x._2 > p).get._1))
    }
  }

  /**
   * @example [0.2, 0.6, 1.8] -> [0.011, 0.099, 0.890] -> [0.011, 0.11, 1.0]
   */
  private def normalizeFitnesses(neuralNetworks: List[(NeuralNetwork, Double)]): List[(NeuralNetwork, Double)] = {
    val sumFitness = neuralNetworks.foldRight(0.0)((curr, prev) => prev + curr._2*curr._2)
    neuralNetworks
      .sortBy(x => x._2)
      .scanLeft((null: NeuralNetwork, 0.0)) {
        case (prev, curr) => (curr._1, prev._2 + curr._2*curr._2/sumFitness)
      }
  }

  /**
   * @example [0.2, 0.6, 1.8, 2.8, 3.6] -> [0/10, 1/10, 2/10, 3/10, 4/10] -> [0/10, 1/10, 3/10, 6/10, 10/10]
   */
  private def rankNormalization(neuralNetworks: List[(NeuralNetwork, Double)]): List[(NeuralNetwork, Double)] = {
    val totalFitness = neuralNetworks.length*(neuralNetworks.length - 1)/2.0
    neuralNetworks
      .sortBy(x => x._2)
      .zipWithIndex
      .map {
        case (element, index) => (element._1, index/totalFitness)
      }
      .scanLeft((null: NeuralNetwork, 0.0)) {
        case (prev, curr) => (curr._1, prev._2 + curr._2)
      }
  }
}