package conways

import processing.core._

object ConwaysGameProcessing extends PApplet {
  def main(args: Array[String]) {
    val game = new ConwaysGame
    val frame = new javax.swing.JFrame("Conway's Game of Life")
    frame.getContentPane().add(game)
    game.init

    frame.pack
    frame.setVisible(true)
  }
}

class ConwaysGame extends PApplet {
  val black = 0
  val white = 255
  val gridSize = 100
  val screenSize = 1000
  
  var current = GameOfLife.init(gridSize, gridSize)
  var started = false
  val boxThickness = screenSize / gridSize
  
  override def setup() = {
    size(screenSize, screenSize)
    background(0)
  }
  
  override def draw() = {
    if (mousePressed && !started) {
      val y = mouseY
      val x = mouseX
      current(y / boxThickness)(x / boxThickness) = true
      fill(white)
      rect(x / boxThickness * boxThickness, y / boxThickness * boxThickness, boxThickness, boxThickness)
    }
    if (keyPressed) {
      started = true
    }
    if (started) {
      for (y <- 0 until gridSize; x <- 0 until gridSize) {
        if (current(y)(x)) {
          fill(white)
          rect(x * boxThickness, y * boxThickness, boxThickness, boxThickness)
        } else {
          fill(black)
          rect(x * boxThickness, y * boxThickness, boxThickness, boxThickness)
        }
      }
      current = GameOfLife.nextGrid(current)
    }
  }
}