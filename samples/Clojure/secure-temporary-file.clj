user=> (doto (java.io.File/createTempFile "pre" ".suff") .deleteOnExit)
#<File /tmp/pre8116759964152254766.suff>
