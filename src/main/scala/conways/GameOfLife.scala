package conways

import scala.collection._

object GameOfLife {
  def nextGrid(grid: Grid) = {
    val newgrid = new Grid(grid.width, grid.height)
    
    for (y <- -1 to grid.height; x <- -1 to grid.width) {
      val numofneighbors = grid.liveNeighbors(new Cell(x, y)).size

      if (numofneighbors < 2) {
        // If a cell is alive but has fewer than 2 live neighbors, it dies of loneliness.
        newgrid(x,y) = false
      } else if (numofneighbors > 3) {
        // If a cell is alive but has more than 3 live neighbors, it dies of overcrowding.
        newgrid(x,y) = false
      } else if (grid(x, y) && (numofneighbors == 3 || numofneighbors == 2)) {
        // If a cell is alive and has exactly 2 or 3 live neighbors, it stays alive.
        newgrid(x,y) = true
      } else if (numofneighbors == 3) {
        // If a cell is dead but has exactly 3 live neighbors, it springs to life.
        newgrid(x,y) = true
      }
    }
    newgrid.fit
    newgrid
  }
}