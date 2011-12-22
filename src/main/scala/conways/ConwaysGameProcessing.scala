package conways

import processing.core._
import java.awt.event._
import javax.swing.JFrame

object ConwaysGameProcessing extends App {
  val game = new ConwaysGame
  val frame = new JFrame("Conway's Game of Life")
  frame.getContentPane().add(game)
  game.init

  frame.pack
  frame.setVisible(true)
  frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
}

class ConwaysGame extends PApplet {
  val black = 0
  val white = 255
  val gridSize = 20
  val screenSize = 1000

  var current = new Grid(gridSize, gridSize)
  var started = false
  val boxThickness = screenSize / gridSize

  override def setup() = {
    frameRate(5)
    size(screenSize, screenSize)
    background(0)
  }

  override def draw() = {
    if (started) {
      current = GameOfLife.nextGrid(current)
    }
    drawGrid()
  }

  def drawGrid() {
    background(0)
    for (y <- 0 until gridSize; x <- 0 until gridSize) {
      if (current(x, y)) {
        fill(white)
        rect(x * boxThickness, y * boxThickness, boxThickness, boxThickness)
      }
    }
  }

  override def mouseDragged(e: MouseEvent) {
    if (!started) {
      val y = e.getY()
      val x = e.getX()
      val roundedCoordinates = new Cell(x / boxThickness, y / boxThickness)
      if (current.inGrid(roundedCoordinates)) {
        current(x / boxThickness, y / boxThickness) = true
      }
    }
  }

  override def keyPressed(e: KeyEvent) {
    started = true
  }
}