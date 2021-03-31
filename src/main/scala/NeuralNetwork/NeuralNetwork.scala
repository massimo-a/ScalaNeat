package NeuralNetwork

import scala.util.Random
import scala.xml._

/**
 * An immutable neural network class. Neural networks are represented as directed
 * acyclic graphs (DAG).
 * @author Massimo Angelillo
 * @param numberOfNeurons The number of nodes in the neural network
 * @param numberOfOutputNeurons The quantity of output neurons
 * @param numberOfInputNeurons The quantity of input neurons
 * @param neurons The list of neurons
 * @param genes The connections between the neurons
 * @param activation The activation function for the neural network
 */
final case class NeuralNetwork
(
  numberOfNeurons: Int,
  numberOfOutputNeurons: Int = 1,
  numberOfInputNeurons: Int = 1,
  neurons: Vector[Neuron] = Vector(),
  genes: Vector[Connection] = Vector(),
  activation: Double => Double = x => {
    Math.exp(x) / (Math.exp(x) + 1)
  }
) {
  val inputNeurons: Vector[Int] = Vector.tabulate(numberOfInputNeurons)(x => x)
  val outputNeurons: Vector[Int] = Vector.tabulate(numberOfOutputNeurons)(x => numberOfInputNeurons + x)

  /**
   * The output of any given neuron is the sum of all outputs
   * of neurons pointing to the current neuron plus its bias.
   * A neuron with no connections pointing to it is an input neuron
   * and therefore gets its output from the user/environment
   * @param neuron Index of the neuron whose output is being calculated
   * @param input When the program reaches an input neuron,
   *              it takes values from this list as its output
   * @return The output of the neuron
   */
  def out(neuron: Int, input: Vector[Double]): Double = {
    val g = genes.filter(x => x.to == neuron)
    //println(g)
    var sum = 0.0
    for(e <- g) {
      sum += e.weight*out(e.from, input)
    }
    if(g.isEmpty) {
      if(inputNeurons.contains(neuron)) {
        input(neuron)
      } else {
        0.0
      }
    } else {
      activation(sum + neurons(neuron).bias)
    }
  }

  /**
   * Recursively checks if there is a path that goes from neuron a to neuron b
   * @param a Index of start node
   * @param b Index of end node
   * @return Boolean indicating whether a path exists
   */
  def hasPath(a: Int, b: Int): Boolean = {
    if(a == b) {
      return true
    }
    val adj = genes.filter(x => x.from == a).map(x => x.to)
    for(c <- adj) {
      if(hasPath(c, b)) {
        return true
      }
    }
    false
  }

  def totalOutput(input: Double*): Vector[Double] = {
    outputNeurons.map(x => out(x, input.toVector))
  }

  /*
   * Mutation functions
   */
  def mutateConnection(i: Int, delta: Double): NeuralNetwork = {
    copy(genes = genes.updated(i, genes(i).copy(weight = genes(i).weight + delta)))
  }

  def mutateBias(i: Int, delta: Double): NeuralNetwork = {
    copy(neurons = neurons.updated(i, neurons(i).copy(bias = neurons(i).bias + delta)))
  }

  def addConnection(node1: Int, node2: Int, weight: Double): NeuralNetwork = {
    copy(genes = genes.prepended(Connection(node1, node2, weight)))
  }

  def addNeuronBetween(c: Connection, b: Double): NeuralNetwork = {
    addNeuronBetween(c.from, c.to, b)
  }

  def addNeuronBetween(node1: Int, node2: Int, b: Double): NeuralNetwork = {
    val n = Neuron(numberOfNeurons, b)
    val w = genes.find(c => c.from == node1 && c.to == node2).get.weight
    copy(genes = genes
      .prepended(Connection(node1, numberOfNeurons, w))
      .prepended(Connection(numberOfNeurons, node2, 1.0))
      .filterNot(pred => pred.from == node1 && pred.to == node2),
      neurons = neurons.prepended(n),
      numberOfNeurons = numberOfNeurons + 1)
  }

  def removeConnection(i: Int): NeuralNetwork = {
    copy(genes = genes.filterNot(x => genes.indexOf(x) == i))
  }

  def mutate(r: Random, p: Double): NeuralNetwork = {
    if(r.nextDouble() < p) {
      val i = if (genes.isEmpty) {
        r.between(0, 2)
      } else {
        r.between(0, 5)
      }
      i match {
        case 0 => mutateBias(r.between(0, neurons.length), r.nextGaussian())
        case 1 =>
          val (node1, node2) = (r.between(0, neurons.length), r.between(0, neurons.length))
          if (validConnection(node1, node2)) {
            addConnection(node1, node2, r.nextGaussian())
          } else {
            copy()
          }
        case 2 => addNeuronBetween(genes(r.between(0, genes.length)), r.nextDouble())
        case 3 => mutateConnection(r.between(0, genes.length), r.nextGaussian())
        case 4 => if(genes.length > 5) {
          removeConnection(r.between(0, genes.length))
        } else {
          copy()
        }
      }
    } else {
      copy()
    }
  }

  def toXML: Elem =
    <neural-network>
      <inputs>{numberOfInputNeurons}</inputs>
      <outputs>{numberOfOutputNeurons}</outputs>
      <neurons>
        {neurons.map(_.toXML)}
      </neurons>
      <connections>
        {genes.map(_.toXML)}
      </connections>
    </neural-network>

  def fromXML(path: String): Unit = {
    val xml = XML.loadFile(path)
    val numInputs = (xml \\ "inputs").text.toInt
    val numOutputs = (xml \\ "outputs").text.toInt

    val neurons = (xml \\ "neuron").map(n => {
      val id = (n \ "id").text.toInt
      val bias = (n \ "bias").text.toDouble
      Neuron(id, bias)
    }).toList

    val connections = (xml \\ "connection").map(c => {
      val from = (c \ "from").text.toInt
      val to = (c \ "to").text.toInt
      val weight = (c \ "weight").text.toDouble
      Connection(from, to, weight)
    }).toList

    copy(
      numberOfNeurons = neurons.length,
      numberOfInputNeurons = numInputs,
      numberOfOutputNeurons = numOutputs,
      neurons = neurons.toVector,
      genes = connections.toVector)
  }

  /**
   * Checks if a connection between two neurons is valid.
   * A connection is valid if:
   *   1. No path exists from end to start (meaning no cycle will form from connecting them)
   *   2. The connection does not already exist
   *   3. The connection does not start at an output neuron
   *   4. The connection does not end at an input neuron
   * @param node1 Index of start node
   * @param node2 Index of end node
   * @return Boolean indicating whether the connection is valid
   */
  private def validConnection(node1: Int, node2: Int): Boolean = {
    !hasPath(node2, node1) &&
      !genes.exists(x => x.from == node1 && x.to == node2) &&
      !outputNeurons.contains(node1) &&
      !inputNeurons.contains(node2)
  }
}
