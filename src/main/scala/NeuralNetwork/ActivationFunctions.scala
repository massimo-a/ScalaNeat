package NeuralNetwork

/**
 * List of popular activation functions for neural networks
 * @author Massimo Angelillo
 */
case object ActivationFunctions {
  /*
   * Range (-1, 1)
   */
  def tanh(a: Double): Double = {
    Math.tanh(a)
  }

  /*
   * Range (0, 1)
   */
  def sigmoid(a: Double): Double = {
    Math.exp(a) / (Math.exp(a) + 1)
  }

  /*
   * Range [0, Inf)
   */
  def relu(a: Double): Double = {
    Math.max(0, a)
  }

  /*
   * Range (-Inf, Inf)
   */
  def identity(a: Double): Double = {
    a
  }

  /*
   * Range [0, Inf)
   */
  def leakyRelu(a: Double): Double = {
    Math.max(0.01*Math.abs(a), a)
  }

  /*
   * Range [0, Inf)
   */
  def paramRelu(b: Double): Double => Double = {
    a: Double => {
      Math.max(b * Math.abs(a), Math.abs(a))
    }
  }

  /*
   * Range (0, Inf)
   */
  def softPlus(a: Double): Double = {
    Math.log(1 + Math.exp(a))
  }

  /*
   * Range (0, 1]
   */
  def gaussian(a: Double): Double = {
    Math.exp(-a*a)
  }

  /*
   * Range (-1, 1)
   */
  def softSign(a: Double): Double = {
    a / (1 + Math.abs(a))
  }
}
