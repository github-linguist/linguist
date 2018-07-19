connection = URL new("https://sourceforge.net") openConnection
scanner = Scanner new(connection getInputStream)

while(scanner hasNext,
  scanner next println
)
