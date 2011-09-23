package conways

import scala.collection.mutable

class Grid(val width: Int, val length: Int) {
  def apply(x: Int, y: Int) = {
    grid(y)(x)
  }
  
  val grid = mutable.IndexedSeq.fill(width, length)(false)
  
  def inGrid(coordinate: (Int, Int)): Boolean = {
    coordinate._1 >= 0 && coordinate._2 >= 0 && coordinate._1 < width && coordinate._2 < length
  }
  
  def liveNeighbors(coordinate: (Int, Int)) = {
    val possibleCoordinatesOfNeighbors = for (
      i <- coordinate._1 - 1 to coordinate._1 + 1;
      j <- coordinate._2 - 1 to coordinate._2 + 1 if ((i, j) != coordinate)
    ) yield (i, j)

    possibleCoordinatesOfNeighbors.filter { neighborCoordinate =>
      inGrid(neighborCoordinate) && grid(neighborCoordinate._1)(neighborCoordinate._2)
    }
  }
}