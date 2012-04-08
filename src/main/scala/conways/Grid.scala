package conways

import scala.collection.mutable

class Grid(minSize: Int) {
  var liveCells = mutable.IndexedSeq[Cell]()
  
  def apply(c: Cell) = {
    liveCells.contains(c)
  }
  
  def update(c: Cell, value: Boolean) {
    if (value) {
      if (!this(c)) {
        liveCells = liveCells :+ c
      }
    } else {
      liveCells = liveCells.filter(_ != c)
    }
  }
  
  def size = {
    val gridWidth = if (liveCells.isEmpty) 0 else liveCells.map(_.x).max + 1
    val width = gridWidth max minSize
    val gridHeight = if (liveCells.isEmpty) 0 else liveCells.map(_.y).max + 1
    val height = gridHeight max minSize
    width max height
  }
  
  def fit {
    if (!liveCells.isEmpty) {
	    val xMin = liveCells.map(_.x).min
	    val yMin = liveCells.map(_.y).min
	    if (xMin < 0 || yMin < 0) {
	      liveCells = liveCells.map(e => Cell(e.x - xMin, e.y - yMin))
	    }
	    if ((xMin > 0 || yMin > 0) && size > minSize) {
	      liveCells = liveCells.map(e => Cell(e.x - (xMin - 1), e.y - (yMin - 1)))
	    }
    }
  }
}


case class Cell(val x: Int, val y: Int)