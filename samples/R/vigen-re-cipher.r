mod1 = function(v, n)
# mod1(1:20, 6)   =>   1 2 3 4 5 6 1 2 3 4 5 6 1 2 3 4 5 6 1 2
    ((v - 1) %% n) + 1

str2ints = function(s)
    as.integer(Filter(Negate(is.na),
        factor(levels = LETTERS, strsplit(toupper(s), "")[[1]])))

vigen = function(input, key, decrypt = F)
   {input = str2ints(input)
    key = rep(str2ints(key), len = length(input)) - 1
    paste(collapse = "", LETTERS[
        mod1(input + (if (decrypt) -1 else 1)*key, length(LETTERS))])}

message(vigen("Beware the Jabberwock, my son! The jaws that bite, the claws that catch!", "vigenerecipher"))
  # WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
message(vigen("WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY", "vigenerecipher", decrypt = T))
  # BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH
