package conways

import scala.collection._

object GameOfLife {
  def nextGrid(grid: Grid) = {
    val newgrid = new Grid(grid.width, grid.length)
    
    for (y <- 0 until grid.length; x <- 0 until grid.width) {
      val numofneighbors = grid.liveNeighbors(new Cell(x, y)).size
      val curcell = grid(x, y)

      if (numofneighbors < 2) {
        // If a cell is alive but has fewer than 2 live neighbors, it dies of loneliness.
        newgrid(x,y) = false
      } else if (numofneighbors > 3) {
        // If a cell is alive but has more than 3 live neighbors, it dies of overcrowding.
        newgrid(x,y) = false
      } else if (curcell && (numofneighbors == 3 || numofneighbors == 2)) {
        // If a cell is alive and has exactly 2 or 3 live neighbors, it stays alive.
        newgrid(x,y) = true
      } else if (numofneighbors == 3) {
        // If a cell is dead but has exactly 3 live neighbors, it springs to life.
        newgrid(x,y) = true
      }
    }
    newgrid
  }
}