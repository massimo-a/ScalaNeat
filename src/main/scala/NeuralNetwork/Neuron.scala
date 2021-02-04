package NeuralNetwork

import scala.xml.Elem

/**
 * @author Massimo Angelillo
 * @param index Id of the neuron, used to find it in a collection
 * @param bias The neuron's bias
 */
case class Neuron(index: Int, bias: Double) {
  def toXML: Elem =
    <neuron>
      <id>{index}</id>
      <bias>{bias}</bias>
    </neuron>
}
