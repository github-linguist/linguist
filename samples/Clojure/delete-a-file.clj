(import '(java.io File))
(.delete (File. "output.txt"))
(.delete (File. "docs"))

(.delete (new File (str (File/separator) "output.txt")))
(.delete (new File (str (File/separator) "docs")))
