def read_matrix(fh)
  matrix = []

  (0..8).each { |i|
    l = fh.readline
    matrix[i] = []
    (0..8).each { |j|
      matrix[i][j] = l[j..j].to_i
    }
  }
  matrix
end

def permissible(matrix, i, j)
  ok = [true] * 9
  # Same as another in the column isn't permissible...
  (0..8).each { |i2|
    next if matrix[i2][j] == 0
    ok[matrix[i2][j] - 1] = false
  }
  # Same as another in the row isn't permissible...
  (0..8).each { |j2|
    next if matrix[i][j2] == 0
    ok[matrix[i][j2] - 1] = false
  }
  # Same as another in the 3x3 block isn't permissible...
  igroup = (i / 3) * 3
  jgroup = (j / 3) * 3
  (igroup..(igroup + 2)).each { |i2|
    (jgroup..(jgroup + 2)).each { |j2|
      next if matrix[i2][j2] == 0
      ok[matrix[i2][j2] - 1] = false
    }
  }
  # Convert to the array format...
  (1..9).select { |i2| ok[i2-1] }
end

def deep_copy_sudoku(matrix)
  matrix.collect { |row| row.dup }
end

def solve_sudoku(matrix)
  loop do
    options = []
    (0..8).each { |i|
      (0..8).each { |j|
        next if matrix[i][j] != 0
        p = permissible(matrix, i, j)
        # If nothing is permissible, there is no solution at this level.
        return nil if p.length == 0
        options.push({:i => i, :j => j, :permissible => p})
      }
    }
    # If the matrix is complete, we have a solution...
    return matrix if options.length == 0

    omin = options.min_by { |x| x[:permissible].length }

    # If there is an option with only one solution, set it and re-check permissibility
    if omin[:permissible].length == 1
      matrix[omin[:i]][omin[:j]] = omin[:permissible][0]
      next
    end

    # We have two or more choices. We need to search both...
    omin[:permissible].each { |v|
      mtmp = deep_copy_sudoku(matrix)
      mtmp[omin[:i]][omin[:j]] = v
      ret = solve_sudoku(mtmp)
      return ret if ret
    }

    # We did an exhaustive search on this branch and nothing worked out.
    return nil
  end
end

def print_matrix(matrix)
  if not matrix
    puts "Impossible"
    return
  end

  border = "+-----+-----+-----+"
  (0..8).each { |i|
    puts border if i%3 == 0
    (0..8).each { |j|
      print(j%3 == 0 ? "|" : " ")
      print(matrix[i][j] == 0 ? "." : matrix[i][j])
    }
    print "|\n"
  }
  puts border
end

matrix = read_matrix(DATA)
print_matrix(matrix)
puts
print_matrix(solve_sudoku(matrix))

__END__
394__267_
___3__4__
5__69__2_
_45___9__
6_______7
__7___58_
_1__67__8
__9__8___
_264__735
