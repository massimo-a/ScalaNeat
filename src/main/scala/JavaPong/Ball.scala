package JavaPong

import java.awt.{Color, Graphics}

import scala.util.Random

/**
 * Ball for pong.
 * @author Massimo Angelillo
 * @param x x position.
 * @param y y position.
 * @param speedX ball movement in the x direction.
 * @param speedY ball movement in the y direction.
 * @param score ball's score.
 * @param size size of the ball.
 */
case class Ball
(x: Double,
 y: Double,
 speedX: Double,
 speedY: Double,
 score: Int = 0,
 size: Int = 15) {
  def draw(g: Graphics): Unit = {
    g.setColor(Color.WHITE)
    g.fillRect(x.toInt, y.toInt, 15, 15)
  }

  def move(): Ball = {
    val x2 = x + speedX
    val y2 = y + speedY

    val speedx = if(x2 <= 0) {
      math.abs(speedX)
    } else if(x2 >= 900) {
      -math.abs(speedX)
    } else {
      speedX
    }

    val speedy = if(y2 <= 15) {
      math.abs(speedY)
    } else if(y2 >= 385) {
      -math.abs(speedY)
    } else {
      speedY
    }

    val s = if(x2 >= 900) {
      score + 1
    } else {
      score
    }

    copy(x2, y2, speedx, speedy, s)
  }
}

case object Ball {
  def init(random: Random, width: Int, height: Int): Ball = {
    val speedx = random.between(1.9, 2.0)
    val speedy = if(random.nextBoolean()) {
      math.sqrt(4 - speedx*speedx)
    } else {
      -math.sqrt(4 - speedx*speedx)
    }
    Ball(width/2.0, height/2.0, speedx, speedy)
  }
}