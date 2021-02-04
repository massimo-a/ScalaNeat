package JavaPong

import java.awt.{Color, Graphics, Rectangle}
import NeuralNetwork.{ActivationFunctions, NeuralNetwork, Neuron}

/**
 * Paddle for Pong.
 * @author Massimo Angelillo
 * @param x x position.
 * @param y y position.
 * @param ball associated ball.
 * @param fitness fitness during the current game.
 * @param brain neural network that decides paddle movement.
 * @param width width of paddle.
 * @param height height of paddle.
 */
case class Paddle
(x: Double,
 y: Double,
 ball: Ball,
 fitness: Double = 0.0,
 brain: NeuralNetwork = NeuralNetwork(
   numberOfOutputNeurons = 2,
   numberOfInputNeurons = 4,
   numberOfNeurons = 6,
   neurons = Vector.tabulate(6)(x => Neuron(x, math.random*2 - 1)),
   activation = ActivationFunctions.softSign),
 width: Int = 10,
 height: Int = 50) {
  def draw(g: Graphics): Unit =  {
    g.setColor(Color.pink)
    g.fillRect(x.toInt, y.toInt, width, height)
  }

  def move(): Paddle = {
    val decide = brain.totalOutput(y - ball.y, x - ball.x, ball.speedX, ball.speedY)
    val y2 = if(decide(0) < decide(1)) {
      y - 1
    } else {
      y + 1
    }

    val y3 = if(y2 <= 15) {
      15
    } else if(y2 >= 340) {
      340
    } else {
      y2
    }
    copy(y = y3)
  }

  def collision(): Paddle = {
    if (new Rectangle(ball.x.toInt, ball.y.toInt, ball.size, ball.size)
      .intersects(new Rectangle(x.toInt, y.toInt, width, height))) {
      val fit = if(ball.speedX < 0) {
        fitness
      } else {
        fitness + 1
      }
      copy(fitness = fit, ball = ball.copy(speedX = -math.abs(ball.speedX)))
    } else {
      copy()
    }
  }

  def update(): Paddle = {
    move().copy(ball = ball.move()).collision()
  }
}