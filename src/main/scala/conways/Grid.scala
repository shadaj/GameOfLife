package conways

import scala.collection.mutable

class Grid(val width: Int, val length: Int) {
  def apply(x: Int, y: Int) = {
    grid(y)(x)
  }
  
  def update(x: Int, y: Int, value: Boolean) {
    grid(y)(x) = value
  }
  
  private val grid = mutable.IndexedSeq.fill(width, length)(false)
  
  def inGrid(coordinate: Cell): Boolean = {
    coordinate.x >= 0 && coordinate.y >= 0 && coordinate.x < width && coordinate.y < length
  }
  
  def liveNeighbors(coordinate: Cell) = {
    val possibleCoordinatesOfNeighbors = for (
      x <- coordinate.x - 1 to coordinate.x + 1;
      y <- coordinate.y - 1 to coordinate.y + 1 if (new Cell(x, y) != coordinate)
    ) yield new Cell(x, y)

    possibleCoordinatesOfNeighbors.filter { neighborCoordinate =>
      inGrid(neighborCoordinate) && this(neighborCoordinate.x,neighborCoordinate.y)
    }
  }
}


case class Cell(val x: Int, val y: Int)