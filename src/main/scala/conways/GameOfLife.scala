package conways

import scala.collection._

object GameOfLife {
  
  def nextGrid(grid: Grid) = {
    val newgrid = new Grid(grid.size)
    
    liveCellsAndNeighbors(grid).foreach{c =>
      val numofneighbors = liveNeighbors(grid, c).size
      if (numofneighbors < 2) {
        // If a cell is alive but has fewer than 2 live neighbors, it dies of loneliness.
        newgrid(c) = false
      } else if (numofneighbors > 3) {
        // If a cell is alive but has more than 3 live neighbors, it dies of overcrowding.
        newgrid(c) = false
      } else if (grid(c) && (numofneighbors == 3 || numofneighbors == 2)) {
        // If a cell is alive and has exactly 2 or 3 live neighbors, it stays alive.
        newgrid(c) = true
      } else if (numofneighbors == 3) {
        // If a cell is dead but has exactly 3 live neighbors, it springs to life.
        newgrid(c) = true
      }
    }
    newgrid.fit
    newgrid
  }
  
  def liveCellsAndNeighbors(grid: Grid) = {
    (grid.liveCells ++ grid.liveCells.map(neighbors(grid, _)).flatten).distinct
  }
  
  def neighbors(grid: Grid, cell: Cell): IndexedSeq[Cell] = {
    for (
      x <- cell.x - 1 to cell.x + 1;
      y <- cell.y - 1 to cell.y + 1 if (Cell(x, y) != cell)
    ) yield Cell(x, y)
  }
  def liveNeighbors(grid: Grid, cell: Cell) = {
    neighbors(grid, cell).filter(grid(_))
  }
}