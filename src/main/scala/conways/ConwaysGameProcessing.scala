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

  var current = new Grid(20)
  var started = false
  var boxThickness = screenSize / current.size

  override def setup() = {
    frameRate(10)
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
    boxThickness = screenSize / current.size
    background(0)
    current.liveCells.foreach {c =>
      fill(white)
      rect(c.x * boxThickness, c.y * boxThickness, boxThickness, boxThickness)
    }
  }

  override def mouseDragged(e: MouseEvent) {
    if (!started) {
      val y = e.getY()
      val x = e.getX()
      val cell = new Cell(x / boxThickness, y / boxThickness)
      current(cell) = true
      
    }
  }

  override def keyPressed(e: KeyEvent) {
    started = true
  }
}