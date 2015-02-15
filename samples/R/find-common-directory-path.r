get_common_dir <- function(paths, delim = "/")
{
  path_chunks <- strsplit(paths, delim)

  i <- 1
  repeat({
    current_chunk <- sapply(path_chunks, function(x) x[i])
    if(any(current_chunk != current_chunk[1])) break
    i <- i + 1
  })
  paste(path_chunks[[1]][seq_len(i - 1)], collapse = delim)

}

# Example Usage:
paths <- c(
  '/home/user1/tmp/coverage/test',
  '/home/user1/tmp/covert/operator',
  '/home/user1/tmp/coven/members')

get_common_dir(paths)           # "/home/user1/tmp"
