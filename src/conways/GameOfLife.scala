package conways

import scala.collection._

object GameOfLife {
  def init(x: Int, y: Int) = {
    mutable.IndexedSeq.fill(x, y)(false)
  }

  def liveNeighborsCount(coordinate: (Int, Int), size: (Int, Int), grid: mutable.IndexedSeq[mutable.IndexedSeq[Boolean]]) = {
    val possibleCoordinatesOfNeighbors = for (
      i <- coordinate._1 - 1 to coordinate._1 + 1;
      j <- coordinate._2 - 1 to coordinate._2 + 1 if ((i, j) != coordinate)
    ) yield (i, j)

    possibleCoordinatesOfNeighbors.filter { neighborCoordinate =>
      inGrid(neighborCoordinate, size) && grid(neighborCoordinate._1)(neighborCoordinate._2)
    }.size
  }

  def inGrid(coordinate: (Int, Int), gridSize: (Int, Int)): Boolean = {
    coordinate._1 >= 0 && coordinate._2 >= 0 && coordinate._1 < gridSize._1 && coordinate._2 < gridSize._2
  }

  def nextGrid(grid: mutable.IndexedSeq[mutable.IndexedSeq[Boolean]]) = {
    val newgrid = init(grid.size, grid(0).size)
    for (y <- 0 until grid.size) {
      val curlist = grid(y)
      for (x <- 0 until curlist.size) {
        val numofneighbors = liveNeighborsCount((y, x), (grid.size, curlist.size), grid)
        val curcell = grid(y)(x)
        // If a cell is alive but has fewer than 2 live neighbors, it dies of loneliness.
        if (curcell == true && numofneighbors < 2) {
          newgrid(y)(x) = false
        }
        // If a cell is alive but has more than 3 live neighbors, it dies of overcrowding.
        if (curcell == true && numofneighbors > 3) {
          newgrid(y)(x) = false
        }
        // If a cell is alive and has exactly 2 or 3 live neighbors, it stays alive.
        if (curcell == true && (numofneighbors == 3 || numofneighbors == 2)) {
          newgrid(y)(x) = true
        }
        // If a cell is dead but has exactly 3 live neighbors, it springs to life.
        if (curcell == false && numofneighbors == 3) {
          newgrid(y)(x) = true
        }
      }
    }
    newgrid
  }
}