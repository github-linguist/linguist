#  See the class definitions and constructors with, e.g.
getClass("pixmapIndexed", package=pixmap)
pixmapIndexed

# Image with all one colour
plot(p1 <- pixmapIndexed(matrix(0, nrow=3, ncol=4), col="red"))

# Image with one pixel specified
cols <- rep("blue", 12); cols[7] <- "red"
plot(p2 <- pixmapIndexed(matrix(1:12, nrow=3, ncol=4), col=cols))

# Retrieve colour of a pixel
getcol <- function(pm, i, j)
{
   pmcol <- pm@col
   dim(pmcol) <- dim(pm@index)
   pmcol[i,j]
}
getcol(p2, 3, 4)  #red
