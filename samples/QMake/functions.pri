# QMake include file that calls some functions
# and does nothing else...

exists(.git/HEAD) {
    system(git rev-parse HEAD >rev.txt)
} else {
    system(echo ThisIsNotAGitRepo >rev.txt)
}
