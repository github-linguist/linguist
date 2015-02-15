stream = OpenRead["file.txt"];
While[a != EndOfFile, Read[stream, Word]];
Close[stream]
