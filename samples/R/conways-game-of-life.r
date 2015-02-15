# Generates a new board - either a random one, sample blinker or gliders, or user specified.
gen.board <- function(type="random", nrow=3, ncol=3, seeds=NULL)
{
    if(type=="random")
    {
       return(matrix(runif(nrow*ncol) > 0.5, nrow=nrow, ncol=ncol))
    } else if(type=="blinker")
    {
       seeds <- list(c(2,1),c(2,2),c(2,3))
    } else if(type=="glider")
    {
       seeds <- list(c(1,2),c(2,3),c(3,1), c(3,2), c(3,3))
    }
    board <- matrix(FALSE, nrow=nrow, ncol=ncol)
    for(k in seq_along(seeds))
    {
      board[seeds[[k]][1],seeds[[k]][2]] <- TRUE
    }
    board
}

# Returns the number of living neighbours to a location
count.neighbours <- function(x,i,j)
{
   sum(x[max(1,i-1):min(nrow(x),i+1),max(1,j-1):min(ncol(x),j+1)]) - x[i,j]
}

# Implements the rulebase
determine.new.state <- function(board, i, j)
{
   N <- count.neighbours(board,i,j)
   (N == 3 || (N ==2 && board[i,j]))
}

# Generates the next interation of the board from the existing one
evolve <- function(board)
{
   newboard <- board
   for(i in seq_len(nrow(board)))
   {
      for(j in seq_len(ncol(board)))
      {
         newboard[i,j] <- determine.new.state(board,i,j)
      }
   }
   newboard
}

# Plays the game.  By default, the board is shown in a plot window, though output to the console if possible.
game.of.life <- function(board, nsteps=50, timebetweensteps=0.25, graphicaloutput=TRUE)
{
   if(!require(lattice)) stop("lattice package could not be loaded")
   nr <- nrow(board)

   for(i in seq_len(nsteps))
   {
      if(graphicaloutput)
      {
         print(levelplot(t(board[nr:1,]), colorkey=FALSE))
      } else print(board)

      Sys.sleep(timebetweensteps)

      newboard <- evolve(board)

      if(all(newboard==board))
      {
         message("board is static")
         break
      } else if(sum(newboard) < 1)
      {
         message("everything is dead")
         break
      } else board <- newboard
   }
   invisible(board)
}

# Example usage
game.of.life(gen.board("blinker"))
game.of.life(gen.board("glider", 18, 20))
game.of.life(gen.board(, 50, 50))
