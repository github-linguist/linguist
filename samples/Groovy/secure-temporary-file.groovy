def file = File.createTempFile( "xxx", ".txt" )
file.deleteOnExit()
println file
