package conways

import scala.collection.mutable

class Grid(minWidth: Int, minHeight: Int) {
  private var liveCells = mutable.IndexedSeq[Cell]()

  
  def apply(x: Int, y: Int) = {
    liveCells.contains(Cell(x,y))
  }
  
  def update(x: Int, y: Int, value: Boolean) {
    if (value) {
      if (!this(x,y)) {
        liveCells = liveCells :+ Cell(x,y)
      }
    } else {
      liveCells = liveCells.filter(_ != Cell(x,y))
    }
  }
  
  def width = {
    val gridWidth = if (liveCells.isEmpty) 0 else liveCells.map(_.x).max + 1
    gridWidth max minWidth
  }
  
  def height = {
    val gridHeight = if (liveCells.isEmpty) 0 else liveCells.map(_.y).max + 1
    gridHeight max minHeight
  }
  
  
  def fit {
    val xMin = liveCells.map(_.x).min
    val yMin = liveCells.map(_.y).min
    if (xMin < 0) {
      liveCells = liveCells.map(e => Cell(e.x - xMin, e.y))
    }
    if (yMin < 0) {
      liveCells = liveCells.map(e => Cell(e.x, e.y - yMin))
    }
  }
  
  def inGrid(coordinate: Cell): Boolean = {
    coordinate.x >= 0 && coordinate.y >= 0 && coordinate.x < width && coordinate.y < height
  }
  
  def liveNeighbors(coordinate: Cell) = {
    val possibleCoordinatesOfNeighbors = for (
      x <- coordinate.x - 1 to coordinate.x + 1;
      y <- coordinate.y - 1 to coordinate.y + 1 if (Cell(x, y) != coordinate)
    ) yield Cell(x, y)

    possibleCoordinatesOfNeighbors.filter { neighborCoordinate =>
      this(neighborCoordinate.x,neighborCoordinate.y)
    }
  }
}


case class Cell(val x: Int, val y: Int)