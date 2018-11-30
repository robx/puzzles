/*
 * Slitherlink with latin square clues
 */
package slither

import jp.kobe_u.copris._
import jp.kobe_u.copris.dsl._
import puzzle._

case class VAMAconn(m: Int, n: Int, board: Seq[Seq[String]]) extends BoardPuzzle {
  def areaName(cell: Cell) = at(cell)
  val areaNames = cells.map(areaName).toSet
  val areaCells =
    areaNames.map(name => name -> cells.filter(cell => areaName(cell) == name).toSet).toMap
  def show(sol: Set[Cell]) {
    for (i <- 0 until m) {
      for (j <- 0 until n)
	if (sol.contains((i,j))) print(" ..")
	else print(" ##")
      println
    }
  }
}

object ConnSolver extends BoardPuzzleSolver[VAMAconn] {
  import scala.math.Ordering.Implicits._

  val name = "vama.Solver"
  var blackComponents: Set[Set[Cell]] = _
  var whiteComponents: Set[Set[Cell]] = _

  def puzzleFactory(m: Int, n: Int, board: Seq[Seq[String]]) =
    VAMAconn(m, n, board)

  def define = {
    for (cell <- puzzle.cells)
      int('x(cell), 0, 1)
    for (cell <- puzzle.cells; cell1 <- puzzle.adjCells(cell)) {
      int('e(cell,cell1), 0, 1)
      add(('e(cell,cell1) === 1) ==> (('x(cell) === 1) && ('x(cell1) === 1)))
    }
    for (cell <- puzzle.cells; cell1 <- puzzle.adjCells(cell); if cell < cell1)
      add('e(cell,cell1) + 'e(cell1,cell) <= 1)
    for ((i,j) <- puzzle.cells) {
      if (i > 0) {
        int('first((i,j)), 0)
      } else {
        int('first((i,j)), 0, 1)
        add(('first((i,j)) === 1) ==> ('x((i,j)) === 1))
        for (j1 <- 0 until j)
          add(('x((i,j1)) === 1) ==> ('first((i,j)) === 0))
      }
    }
    for (cell <- puzzle.cells) {
      val in = puzzle.adjCells(cell).map(cell1 => 'e(cell1,cell))
      int('in(cell), 0, 1)
      add('in(cell) === Add(in))
      add(('first(cell) === 1) ==> ('in(cell) === 0))
      add((('first(cell) === 0) && ('x(cell) === 1)) ==> ('in(cell) === 1))
    }
    for (cell <- puzzle.cells) {
      int('dist(cell), 0, 2 * puzzle.n)
    }
    for (cell <- puzzle.cells) {
      add(('first(cell) === 1) ==> ('dist(cell) === 0))
      for (cell1 <- puzzle.adjCells(cell)) {
        add(('e(cell,cell1) === 1) ==> ('dist(cell) + 1 === 'dist(cell1)))
      }
    }
    for (area <- puzzle.areaNames) {
      val xs = puzzle.areaCells(area).map(cell => 'x(cell))
      add(Add(xs) === 2)
    }
    for (row <- puzzle.rows)
      add(Add(row.map(cell => 'x(cell))) === 2)
    for (col <- puzzle.cols)
      add(Add(col.map(cell => 'x(cell))) === 2)
  }

  def checkSolution: Boolean = {
    val nodes = puzzle.cells.filter(cell => solution('x(cell)) == 1).toSet
    def nextCells(cell: Cell) =
      puzzle.adjCells(cell).filter(cell1 => solution('x(cell1)) == 1).toSet
    val arcs = nodes.map(cell => cell -> nextCells(cell)).toMap
    blackComponents = getComponents(nodes, arcs)
    if (verbose >= 2)
      println("Components = " + blackComponents.size)
    if (verbose >= 3) {
      val sol = puzzle.cells.filter(cell => solution('x(cell)) == 0).toSet
      puzzle.show(sol)
    }
    true
    //blackComponents.size == 1
  }
  def addNegation {
    val nodes = puzzle.cells.filter(cell => solution('x(cell)) == 0).toSet
    def nextCells(cell: Cell) =
      puzzle.adjCells(cell).filter(cell1 => solution('x(cell1)) == 0).toSet
    val arcs = nodes.map(cell => cell -> nextCells(cell)).toMap
    whiteComponents = getComponents(nodes, arcs)
    for (whites <- whiteComponents) {
      val blacks = for (cell <- whites; cell1 <- puzzle.adjCells(cell); if solution('x(cell1)) > 0)
                   yield cell1
      add(Or(whites.map(cell => 'x(cell) > 0)) || Or(blacks.map(cell => 'x(cell) === 0)))
    }
  }

  override def findFirstSolution = findIncremental(true, checkSolution, addNegation)
  override def findNextSolution = findIncremental(false, checkSolution, addNegation)

  def showSolution {
    val sol = puzzle.cells.filter(cell => solution('x(cell)) == 0).toSet
    val fst = puzzle.cells.filter(cell => solution('first(cell)) == 1).toSet
    val tree = for (cell <- puzzle.cells; cell1 <- puzzle.adjCells(cell); if solution('e(cell,cell1)) == 1) yield (cell, cell1)
    if (quiet == 0) {
      println("Solution = " + sol)
      println("Size = " + sol.size)
      println("First = " + fst)
      println("Tree = " + tree)
      puzzle.show(sol)
    }
  }
}
