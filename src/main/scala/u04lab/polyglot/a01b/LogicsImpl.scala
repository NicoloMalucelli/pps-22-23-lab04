package u04lab.polyglot.a01b

import u04lab.polyglot.{OptionToOptional, Pair}
import u04lab.code.Option
import u04lab.code.Option.*
import u04lab.code.List
import u04lab.code.List.*
//import u04lab.code.Stream.*
import u04lab.polyglot.Pair.*

//import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
class LogicsImpl(private val size: Int, private val mines: Int) extends Logics:
  private val grid: Grid = Grid(size, mines)

  def hit(x: Int, y: Int): java.util.Optional[Integer] =
    val clickedCell = Option.get(grid.getCell(Pair(x, y)))
    if clickedCell.hasMine then
      java.util.Optional.empty()
    else {
        clickedCell.clicked = true
        java.util.Optional.of(length(filter(grid.getAdjacentCells(clickedCell.position))(_.hasMine)))
    }

  def won = length(grid.getClickedCells()) + mines == size*size


private trait Cell:
  def position: Pair[Int, Int]
  def hasMine: Boolean
  def hasMine_=(state: Boolean): Unit
  def clicked : Boolean
  def clicked_=(state: Boolean): Unit

object Cell:
  private case class CellImpl(
     position: Pair[Int, Int],
     override var hasMine: Boolean,
     override var clicked: Boolean) extends Cell {}
  def apply(position: Pair[Int, Int]): Cell = CellImpl(position, false, false)

private trait Grid:
  def getCell(pos: Pair[Int, Int]): Option[Cell]
  def getClickedCells(): List[Cell]
  def getAdjacentCells(pos: Pair[Int, Int]): List[Cell]

object Grid:
  private case class GridImpl(size: Int, mines: Int) extends Grid:
    var cells: List[Cell] = empty

    for i <- 0 until size
        j <- 0 until size
    do cells = append(cells, cons(Cell(Pair(i, j)), empty))

    List.apply(List.getRandoms(cells)(mines)){_.hasMine = true}

    override def getCell(pos: Pair[Int, Int]): Option[Cell] = filter(cells)(c => c.position == pos) match
      case List.Cons(h: Cell, _) => Option.Some(h)
      case List.Nil() => Option.None()

    override def getClickedCells(): List[Cell] = filter(cells)(c => c.clicked)

    override def getAdjacentCells(position: Pair[Int, Int]): List[Cell] =
      List.filter(cells)(cell => Math.abs(position.getX - cell.position.getX) <= 1 &&  Math.abs(position.getY - cell.position.getY) <= 1 && !position.equals(cell.position))


  def apply(size: Int, mines: Int): Grid = GridImpl(size, mines)