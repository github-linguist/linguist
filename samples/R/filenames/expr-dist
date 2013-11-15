#!/usr/bin/env Rscript

# Copyright (c) 2013 Daniel S. Standage, released under MIT license
#
# expr-dist: plot distributions of expression values before and after
#            normalization; visually confirm that normalization worked
#            as expected
#
# Program input is a matrix of expression values, each row corresponding to a
# molecule (gene, transcript, etc) and each row corresponding to that molecule's
# expression level or abundance. The program expects the rows and columns to be
# named, and was tested primarily on output produced by the
# 'rsem-generate-data-matrix' script distributed with the RSEM package.
#
# The program plots the distributions of the logged expression values by sample
# as provided, then normalizes the values, and finally plots the distribution of
# the logged normalized expression values by sample. The expectation is that all
# samples' distributions will have a similar shape but different medians prior
# to normalization, and that post normalization they will all have an identical
# median to facilitate cross-sample comparison.


# MedianNorm function borrowed from the EBSeq library version 1.1.6
# See http://www.bioconductor.org/packages/devel/bioc/html/EBSeq.html
MedianNorm <- function(data)
{
  geomeans <- exp( rowMeans(log(data)) )
  apply(data, 2, function(cnts) median((cnts/geomeans)[geomeans > 0]))
}

library("getopt")
print_usage <- function(file=stderr())
{
  cat("
expr-dist: see source code for full description
Usage: expr-dist [options] < expr-matrix.txt
  Options:
    -h|--help:          print this help message and exit
    -o|--out: STRING    prefix for output files; default is 'expr-dist'
    -r|--res: INT       resolution (dpi) of generated graphics; default is 150
    -t|--height: INT    height (pixels) of generated graphics; default is 1200
    -w|--width: INT     width (pixels) of generated graphics; default is 1200
    -y|--ylim: REAL     the visible range of the Y axis depends on the first
                        distribution plotted; if other distributions are getting
                        cut off, use this setting to override the default\n\n")
}

spec <- matrix( c("help",   'h', 0, "logical",
                  "out",    'o', 1, "character",
                  "res",    'r', 1, "integer",
                  "height", 't', 1, "integer",
                  "width",  'w', 1, "integer",
                  "ylim",   'y', 1, "double"),
                byrow=TRUE, ncol=4)
opt  <- getopt(spec)
if(!is.null(opt$help))
{
  print_usage(file=stdout())
  q(status=1)
}
if(is.null(opt$height)) { opt$height <- 1200           }
if(is.null(opt$out))    { opt$out    <- "expr-dist"    }
if(is.null(opt$res))    { opt$res    <- 150            }
if(is.null(opt$width))  { opt$width  <- 1200           }
if(!is.null(opt$ylim))  { opt$ylim   <- c(0, opt$ylim) }

# Load data, determine number of samples
data  <- read.table(file("stdin"), header=TRUE, sep="\t", quote="")
nsamp <- dim(data)[2] - 1
data  <- data[,1:nsamp+1]

# Plot distribution of expression values before normalization
outfile <- sprintf("%s-median.png", opt$out)
png(outfile, height=opt$height, width=opt$width, res=opt$res)
h <- hist(log(data[,1]), plot=FALSE)
plot(h$mids, h$density, type="l", col=rainbow(nsamp)[1], main="",
     xlab="Log expression value", ylab="Proportion of molecules", ylim=opt$ylim)
for(i in 2:nsamp)
{
  h <- hist(log(data[,i]), plot=FALSE)
  lines(h$mids, h$density, col=rainbow(nsamp)[i])
}
devnum <- dev.off()

# Normalize by median
size.factors <- MedianNorm(data.matrix(data))
data.norm <- t(apply(data, 1, function(x){ x / size.factors }))

# Plot distribution of normalized expression values
outfile <- sprintf("%s-median-norm.png", opt$out)
png(outfile, height=opt$height, width=opt$width, res=opt$res)
h <- hist(log(data.norm[,1]), plot=FALSE)
plot(h$mids, h$density, type="l", col=rainbow(nsamp)[1], main="",
     xlab="Log normalized expression value", ylab="Proportion of molecules",
     ylim=opt$ylim)
for(i in 2:nsamp)
{
  h <- hist(log(data.norm[,i]), plot=FALSE)
  lines(h$mids, h$density, col=rainbow(nsamp)[i])
}
devnum <- dev.off()
