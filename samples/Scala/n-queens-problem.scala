case class Pos(row: Int, column: Int) {
  def sameRow(p: Pos) = row == p.row
  def sameColumn(p: Pos) = column == p.column
  def sameDiag(p: Pos) = (p.column - column).abs == (p.row - row).abs
  def illegal(p: Pos) = sameRow(p) || sameColumn(p) || sameDiag(p)
  def legal(p: Pos) = !illegal(p)
}

def rowSet(size: Int, row: Int) = Iterator.tabulate(size)(column => Pos(row, column))

def expand(solutions: Iterator[List[Pos]], size: Int, row: Int) =
  for {
    solution <- solutions
    pos <- rowSet(size, row)
    if solution forall (_ legal pos)
  } yield pos :: solution

def seed(size: Int) = rowSet(size, 0) map (sol => List(sol))

def solve(size: Int) = (1 until size).foldLeft(seed(size)) (expand(_, size, _))
