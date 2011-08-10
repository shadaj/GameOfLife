package conways

import scala.collection._

object GameOfLife {
  def init(x: Int, y: Int, trues: (Int, Int)*) = {
    val grid = mutable.IndexedSeq.fill(x, y)(false)
    trues.foreach { case (a, b) => grid(a)(b) = true }
    grid
  }

  def liveNeighborsCount(coordinates: (Int, Int), size: (Int, Int), grid: mutable.IndexedSeq[mutable.IndexedSeq[Boolean]]) = {
    val possicoordinates = for (
      i <- coordinates._1 - 1 to coordinates._1 + 1;
      j <- coordinates._2 - 1 to coordinates._2 + 1 if ((i, j) != coordinates)
    ) yield (i, j)
    possicoordinates.filter { c =>
      c._1 >= 0 && c._2 >= 0 && c._1 < size._1 && c._2 < size._2 && grid(c._1)(c._2)
    }.size
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