package PacMan

import NeuralNetwork.{NeuralNetwork, Neuron}
import nintaco.api._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Main {
  ApiSource.initRemoteAPI("localhost", 9999)
  private val api = ApiSource.getAPI
  private var population = Population(10, api)
  private var startPressed = false

  def main(args: Array[String]): Unit = {
    launchNintaco()
  }

  val renderFinished: FrameListener = () => {
    if(api.peekCPU(0x000000) == 0x20) {
      api.writeGamepad(0, GamepadButtons.Start, true)
      if(!startPressed) {
        startPressed = true
        population = population.increment()
        println("CURRENT NN IN GENERATION " + population.current)
      }
    } else {
      population.move()
      if(startPressed) {
        startPressed = false
      }
    }
  }

  private def launchNintaco(): Unit = {
    api.addFrameListener(renderFinished)
    api.addStatusListener((m: String) => println(s"Status message: $m"))
    api.addActivateListener(() => {
      println("API enabled")
      api.setSpeed(0)
    })
    api.addDeactivateListener(() => println("API disabled"))
    api.addStopListener(() => println("API stopped"))
    api.run()
  }
}

case class Population(size: Int, population: Vector[(NeuralNetwork, Int)], api: API, current: Int = 0) {
  private val PACMAN: (Int, Int) = (0x000703, 0x000704)
  private val RED: (Int, Int) = (0x001713, 0x001714)
  private val PINK: (Int, Int) = (0x001723, 0x001724)
  private val BLUE: (Int, Int) = (0x001733, 0x001734)
  private val BROWN: (Int, Int) = (0x001743, 0x001744)

  def move(): Unit = {
    val choice = decide()
    choice match {
      case 0 =>
        api.writeGamepad(0, GamepadButtons.Up, true)
      case 1 =>
        api.writeGamepad(0, GamepadButtons.Down, true)
      case 2 =>
        api.writeGamepad(0, GamepadButtons.Right, true)
      case 3 =>
        api.writeGamepad(0, GamepadButtons.Left, true)
    }
  }

  def increment(): Population = {
    if(current == size) {
      selectAndMutate()
    }
    val pacman = (population(current)._1, getScore)
    println(getScore)
    copy(population = population.updated(current, pacman), current = current + 1)
  }

  private def decide(): Int = {
    val (x, y) = (api.peekCPU(PACMAN._1)/256.0, api.peekCPU(PACMAN._2)/256.0)
    val (redx, redy) = (api.peekCPU(RED._1)/256.0 - x, api.peekCPU(RED._2)/256.0 - y)
    val (pinkx, pinky) = (api.peekCPU(PINK._1)/256.0 - x, api.peekCPU(PINK._2)/256.0 - y)
    val (bluex, bluey) = (api.peekCPU(BLUE._1)/256.0 - x, api.peekCPU(BLUE._2)/256.0 - y)
    val (brownx, browny) = (api.peekCPU(BROWN._1)/256.0 - x, api.peekCPU(BROWN._2)/256.0 - y)
    val out = population(current)._1.totalOutput(redx, redy, pinkx, pinky, bluex, bluey, brownx, browny)
    out.indexOf(out.max)
  }

  private def getScore: Int = {
    val tens = api.peekCPU(0x0070)
    val hundredths = api.peekCPU(0x0071)
    val thousandths = api.peekCPU(0x0072)
    val tenThousandths = api.peekCPU(0x0073)
    val hundredThousandths = api.peekCPU(0x0074)
    val millions = api.peekCPU(0x0075)
    val tenMillions = api.peekCPU(0x0076)
    tens * 10 + hundredths * 100 + thousandths * 1000 +
      tenThousandths * 10000 + hundredThousandths * 100000 +
      millions * 1000000 + tenMillions * 10000000
  }

  def selectAndMutate(): Population = {
    val bestPacMen = population
      .sortBy(p => p._2)(Ordering[Int].reverse)
      .take(size/3)
      .map(p => {
        (p._1, 0)
      })

    var pacPop = ArrayBuffer[(NeuralNetwork, Int)]()
    for(p <- bestPacMen) {
      for(_ <- 0 until 3) {
        val pacman = p._1.mutate(new Random(), 0.7)
        pacPop = pacPop.prepend((pacman, 0))
      }
    }
    Population(size, bestPacMen.appendedAll(pacPop), api)
  }
}

case object Population {
  def apply(size: Int, api: API): Population = {
    val brains = Vector.fill(size)(null).map(_ => (Population.initNN(10, new Random()), 0))
    Population(size, brains, api)
  }

  @tailrec
  private def initNN(
   max: Int,
   r: Random,
   accu: NeuralNetwork = NeuralNetwork(
     numberOfNeurons = 12,
     numberOfOutputNeurons = 4,
     numberOfInputNeurons = 8,
     neurons = Vector.tabulate(14)(x => Neuron(x, math.random*2 - 1))),
   i: Int = 0): NeuralNetwork = {
    if(i == max) return accu
    Population.initNN(max, r, accu.mutate(r, 0.95), i+1)
  }
}