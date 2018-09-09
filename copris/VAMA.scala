/*
 * Vama Solver in Copris
 */
package vama

import jp.kobe_u.copris._
import jp.kobe_u.copris.dsl._
import puzzle._

case class VAMA(m: Int, n: Int, board: Seq[Seq[String]]) extends BoardPuzzle {
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

object Solver extends BoardPuzzleSolver[VAMA] {
  import scala.math.Ordering.Implicits._

  val name = "vama.Solver"
  var blackComponents: Set[Set[Cell]] = _
  var whiteComponents: Set[Set[Cell]] = _

  def puzzleFactory(m: Int, n: Int, board: Seq[Seq[String]]) =
    VAMA(m, n, board)

  def define = {
    for (cell <- puzzle.cells)
      int('x(cell), 0, 1)
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
    blackComponents.size == 1
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
    if (quiet == 0) {
      println("Solution = " + sol)
      println("Size = " + sol.size)
      puzzle.show(sol)
    }
  }
}
