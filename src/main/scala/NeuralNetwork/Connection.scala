package NeuralNetwork

import scala.xml.Elem

/**
 * @author Massimo Angelillo
 * @param from Index of the node the connection is coming from
 * @param to Index of the node the connection is going to
 * @param weight The weight of the connection
 */
case class Connection(from: Int, to: Int, weight: Double) {
  def toXML: Elem =
    <connection>
      <from>{from}</from>
      <to>{to}</to>
      <weight>{weight}</weight>
    </connection>
}

