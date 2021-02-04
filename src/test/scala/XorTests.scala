import NeuralNetwork.{ActivationFunctions, NeuralNetwork, Neuron}
import org.scalatest.FunSuite

import scala.util.Random

class XorTests extends FunSuite {
  val selectionSize: Int = 10
  val minimumFitness: Double = 0.999
  val mutationProbability: Double = 0.2
  val rand: Random = new Random(200)

  def runTraining(neuralNetworks: List[NeuralNetwork]): NeuralNetwork = {
    var maxFit: (NeuralNetwork, Double) = (null, 0.0)
    var nn = neuralNetworks
    for(_ <- 0 until 40000) {
      val fitnesses = fitness(nn)
      nn = reproduceAndMutate(select(fitnesses))
      maxFit = fitnesses.maxBy(x => x._2)
    }
    maxFit._1
  }

  def select(neuralNetworks: List[(NeuralNetwork, Double)]): List[NeuralNetwork] = {
    neuralNetworks
      .sortBy(x => x._2)(Ordering[Double].reverse)
      .take(selectionSize)
      .map(x => x._1)
  }

  def reproduceAndMutate(nn: List[NeuralNetwork]): List[NeuralNetwork] = {
    val numChildren = nn.length/selectionSize
    var res = nn
    for(n <- nn) {
      for(_ <- 0 until numChildren) {
        res = res.prepended(n.mutate(rand, mutationProbability))
      }
    }
    res
  }

  def fitness(neuralNetworks: List[NeuralNetwork]): List[(NeuralNetwork, Double)] = {
    val fitnesses = Array.tabulate(neuralNetworks.length)(_ => 1.0)
    for (i <- neuralNetworks.indices) {
      for(j <- 0 to 1; k <- 0 to 1) {
        val x = neuralNetworks(i).totalOutput(j, k).head
        fitnesses(i) -= (x - j.^(k))*(x - j.^(k))
      }
    }
    neuralNetworks.zip(fitnesses)
  }

  def create(): NeuralNetwork = {
    NeuralNetwork(
      numberOfInputNeurons = 2,
      numberOfNeurons = 3,
      neurons = Vector.tabulate(3)(x => Neuron(x, x*0.5)),
      activation = ActivationFunctions.sigmoid)
  }

  val bestNN: NeuralNetwork = runTraining(List.tabulate(200)(_ => create()))

  test("XorTest.00") {
    assert(
      math.abs(bestNN.totalOutput(0, 0).head) < 0.05
    )
  }

  test("XorTest.01") {
    assert(
      math.abs(bestNN.totalOutput(0, 1).head - 1) < 0.05
    )
  }

  test("XorTest.10") {
    assert(
      math.abs(bestNN.totalOutput(1, 0).head - 1.0) < 0.05
    )
  }

  test("XorTest.11") {
    assert(
      math.abs(bestNN.totalOutput(1, 1).head) < 0.05
    )
  }

  println("XOR TESTS:")
  println(s"0, 0: ${bestNN.totalOutput(0, 0).head}")
  println(s"1, 1: ${bestNN.totalOutput(1, 1).head}")
  println(s"0, 1: ${bestNN.totalOutput(0, 1).head}")
  println(s"1, 0: ${bestNN.totalOutput(1, 0).head}")
}