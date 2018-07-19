// *** print *.txt files in current directory
 new File('.').eachFileMatch(~/.*\.txt/) {
   println it
 }

 // *** print *.txt files in /foo/bar
 new File('/foo/bar').eachFileMatch(~/.*\.txt/) {
   println it
 }
