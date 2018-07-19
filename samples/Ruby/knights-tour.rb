# Solve a Knight's Tour
#
#  Nigel_Galloway
#  May 6th., 2012.

class Cell
  Adjust = [[-1,-2],[-2,-1],[-2,1],[-1,2],[1,2],[2,1],[2,-1],[1,-2]]
  def initialize(row=0, col=0, value=nil)
    @adj = Adjust.map{|r,c| [row+r,col+c]}
    @t = false
    $zbl[value] = false unless value.nil?
    @value = value
  end
  def try(value=1)
    return true  if value > $e
    return false if @t
    return false if @value > 0 and @value != value
    return false if @value == 0 and not $zbl[value]
    @t = true
    h = Hash.new
    @adj.each_with_index{|(row, col), n|
      cell = $board[row][col]
      h[cell.wdof*100+n] = cell  if cell.value
    }
    h.sort.each{|key,cell|
      if cell.try(value+1)
        @value = value
        return true
      end
    }
    @t = false
  end
  def wdon
    (@value.nil? or @value > 0 or @t) ? 0 : 1
  end
  def wdof
    @adj.inject(0){|res, (row, col)| res += $board[row][col].wdon}
  end
  attr_reader :value
end

def knight_tour(rows, cols, x, y)
  $e = rows * cols
  $zbl = Array.new($e+1,true)
  $board = Array.new(rows+2) do |i|
    Array.new(cols+2) do |j|
      (i<rows and j<cols) ? Cell.new(i,j,0) : Cell.new
    end
  end
  $board[x][y].try
  rows.times{|r| cols.times{|c| printf("%3s",$board[r][c].value)}; puts}
end

knight_tour(8,8,3,1)
