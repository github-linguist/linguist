import java.io.File

val dir = new File("/foo/bar").list()
dir.filter(file => file.endsWith(".mp3")).foreach(println)
