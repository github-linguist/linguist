['/', './'].each{|dir|
  Dir.mkdir(dir + 'docs')      # create '/docs', then './docs'
  File.open(dir + 'output.txt', 'w') {}  # create empty file /output.txt, then ./output.txt
}
