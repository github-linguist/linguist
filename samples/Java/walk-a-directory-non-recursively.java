File dir = new File("/foo/bar");

String[] contents = dir.list();
for (String file : contents)
    if (file.endsWith(".mp3"))
        System.out.println(file);
