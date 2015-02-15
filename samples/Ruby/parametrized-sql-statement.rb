require 'sqlite3'

db = SQLite3::Database.new(":memory:")

# setup
db.execute('create temp table players (name, score, active, jerseyNum)')
db.execute('insert into players values ("name",0,"false",99)')
db.execute('insert into players values ("name",0,"false",100)')
db.execute('insert into players values ("name",0,"false",101)')

# demonstrate parameterized SQL

# example 1 -- simple placeholders
db.execute('update players set name=?, score=?, active=? where jerseyNum=?', 'Smith, Steve', 42, true, 99)

# example 2 -- named placeholders
db.execute('update players set name=:name, score=:score, active=:active where jerseyNum=:num',
    :num => 100,
    :name => 'John Doe',
    :active => false,
    :score => -1
)

# example 3 -- numbered placeholders
stmt = db.prepare('update players set name=?4, score=?3, active=?2 where jerseyNum=?1')
stmt.bind_param(1, 101)
stmt.bind_param(2, true)
stmt.bind_param(3, 3)
stmt.bind_param(4, "Robert'; DROP TABLE players--")
stmt.execute

# and show the results
db.execute2('select * from players') {|row| p row}
