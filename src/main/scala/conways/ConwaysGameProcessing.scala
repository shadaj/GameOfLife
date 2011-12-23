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
  val screenSize = 1000

  var current = new Grid(20,20)
  var started = false
  var boxThicknessX = screenSize / current.width
  var boxThicknessY = screenSize / current.height

  override def setup() = {
    frameRate(5)
    size(screenSize, screenSize)
    background(0)
  }

  override def draw() {
    if (started) {
      current = GameOfLife.nextGrid(current)
    }
    drawGrid()
  }

  def drawGrid() {
    boxThicknessX = screenSize / current.width
    boxThicknessY = screenSize / current.height
    background(0)
    for (y <- 0 until current.height; x <- 0 until current.width) {
      if (current(x, y)) {
        fill(white)
        rect(x * boxThicknessX, y * boxThicknessY, boxThicknessX, boxThicknessY)
      }
    }
  }

  override def mouseDragged(e: MouseEvent) {
    if (!started) {
      val y = e.getY()
      val x = e.getX()
      val roundedCoordinates = new Cell(x / boxThicknessX, y / boxThicknessY)
      if (current.inGrid(roundedCoordinates)) {
        current(x / boxThicknessX, y / boxThicknessY) = true
      }
    }
  }

  override def keyPressed(e: KeyEvent) {
    started = true
  }
}