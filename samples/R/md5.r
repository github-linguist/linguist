library(digest)
hexdigest <- digest("The quick brown fox jumped over the lazy dog's back",
                    algo="md5", serialize=FALSE)
