(import '(java.io File)
        '(java.util Date))

(Date. (.lastModified (File. "output.txt")))
(Date. (.lastModified (File. "docs")))

(.setLastModified (File. "output.txt")
                  (.lastModified (File. "docs")))
