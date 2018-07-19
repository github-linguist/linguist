declare
  [Sqlite] = {Module.link ['x-ozlib:/sqlite/Sqlite.ozf']}

  DB = {Sqlite.open 'test.db'}
in
  try

     {Sqlite.exec DB
      "CREATE TABLE address ("
      #"addrID		INTEGER PRIMARY KEY,"
      #"addrStreet	TEXT NOT NULL,"
      #"addrCity	TEXT NOT NULL,"
      #"addrState	TEXT NOT NULL,"
      #"addrZIP		TEXT NOT NULL"
      #")" _}

  catch E then
     {Inspector.configure widgetShowStrings true}
     {Inspect E}
  finally
     {Sqlite.close DB}
  end
