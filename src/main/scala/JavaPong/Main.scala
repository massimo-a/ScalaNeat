package JavaPong

import java.awt.{Color, Dimension, Graphics, Image}

import javax.swing.{JFrame, WindowConstants}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Main {
  val rand = new Random(100)
  val gWidth = 900
  val gHeight = 400
  val m = new Main()
  var generation = 0
  var bestPaddle: Paddle = _

  def main(args: Array[String]): Unit = {
    m.run(1)
    while(generation < 10) {
      generation += 1
      bestPaddle = m.paddles
        .sortBy(p => p.fitness - p.ball.score)(Ordering[Double].reverse)
        .head
      println(s"GENERATION : $generation", s"HIGH SCORE : ${bestPaddle.fitness - bestPaddle.ball.score}")
      m.paddles = selectAndMutate(m.paddles)
      m.run(1)
    }
    bestPaddle = m.paddles
      .sortBy(p => p.fitness - p.ball.score)(Ordering[Double].reverse)
      .head
    scala.xml.XML.save("PongNN.xml", bestPaddle.brain.toXML)
    m.paddles = m.paddles
      .sortBy(p => p.fitness - p.ball.score)(Ordering[Double].reverse)
      .take(1)
      .map(p => {
        p.copy(fitness = 0, ball = Ball.init(rand, gWidth, gHeight))
      })
    m.run(3)
    println(s"GENERATION : $generation", s"HIGH SCORE : ${m.paddles.head.fitness - m.paddles.head.ball.score}")
  }

  def selectAndMutate(paddles: ArrayBuffer[Paddle]): ArrayBuffer[Paddle] = {
    val bestPaddles = paddles
      .sortBy(p => p.fitness - p.ball.score)(Ordering[Double].reverse)
      .take(10)
      .map(p => {
        p.copy(fitness = 0, ball = Ball.init(rand, gWidth, gHeight))
      })

    var paddlePop = ArrayBuffer[Paddle]()
    for(p <- bestPaddles) {
      for(_ <- 0 until 20) {
        val b = Ball.init(rand, gWidth, gHeight)
        val paddle = Paddle(gWidth - 20, gHeight/2.0, b, brain = p.brain.mutate(rand, 0.1))
        paddlePop = paddlePop.prepend(paddle)
      }
    }
    bestPaddles.addAll(paddlePop)
  }
}

class Main extends JFrame {
  private val screenSize = new Dimension(Main.gWidth, Main.gHeight)
  private var dbImage: Image = _
  private var dbGraphics: Graphics = _
  private val rand = new Random(200)

  var paddles: ArrayBuffer[Paddle] =
    ArrayBuffer.tabulate(200)(_ => Paddle(Main.gWidth - 20, Main.gHeight/2.0, Ball.init(rand, Main.gWidth, Main.gHeight)))

  this.setTitle("Pong!")
  this.setSize(screenSize)
  this.setResizable(false)
  this.setVisible(true)
  this.setBackground(Color.DARK_GRAY)
  this.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

  override def paint(g: Graphics): Unit = {
    dbImage = createImage(getWidth, getHeight)
    dbGraphics = dbImage.getGraphics
    draw(dbGraphics)
    g.drawImage(dbImage, 0, 0, this)
  }

  def draw(g: Graphics): Unit = {
    paddles.head.draw(g)
    paddles.head.ball.draw(g)
    repaint()
  }

  def update(): ArrayBuffer[Paddle] = {
    paddles.map(_.update())
  }

  def run(i: Int = 0): Unit = {
    while(paddles.exists(p => p.ball.score <= 5 && p.fitness <= 5)) {
      paddles = update()
      Thread.sleep(i)
    }
  }
}