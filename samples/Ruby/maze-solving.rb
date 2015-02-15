class Maze
  # Solve via breadth-first algorithm.
  # Each queue entry is a path, that is list of coordinates with the
  # last coordinate being the one that shall be visited next.
  def solve

    # Clean up.
    reset_visiting_state

    # Enqueue start position.
    @queue = []
    enqueue_cell([], @start_x, @start_y)

    # Loop as long as there are cells to visit and no solution has
    # been found yet.
    path = nil
    until path || @queue.empty?
      path = solve_visit_cell
    end

    if path
      # Mark the cells that make up the shortest path.
      for x, y in path
        @path[x][y] = true
      end
    else
      puts "No solution found?!"
    end
  end

  private

  # Maze solving visiting method.
  def solve_visit_cell
    # Get the next path.
    path = @queue.shift
    # The cell to visit is the last entry in the path.
    x, y = path.last

    # Have we reached the end yet?
    return path  if x == @end_x && y == @end_y

    # Mark cell as visited.
    @visited[x][y] = true

    for dx, dy in DIRECTIONS
      if dx.nonzero?
        # Left / Right
        new_x = x + dx
        if move_valid?(new_x, y) && !@vertical_walls[ [x, new_x].min ][y]
          enqueue_cell(path, new_x, y)
        end
      else
        # Top / Bottom
        new_y = y + dy
        if move_valid?(x, new_y) && !@horizontal_walls[x][ [y, new_y].min ]
          enqueue_cell(path, x, new_y)
        end
      end
    end

    nil         # No solution yet.
  end

  # Enqueue a new coordinate to visit.
  def enqueue_cell(path, x, y)
    # Add new coordinates to the current path and enqueue the new path.
    @queue << path + [[x, y]]
  end
end

# Demonstration:
maze = Maze.new 20, 10
maze.solve
maze.print
