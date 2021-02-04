package NeuralNetwork

trait Agent {
  val brain: NeuralNetwork
  val fitness: Double
}

case class Population(agents: List[Agent])