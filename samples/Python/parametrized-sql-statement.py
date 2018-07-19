import sqlite3

db = sqlite3.connect(':memory:')

# setup
db.execute('create temp table players (name, score, active, jerseyNum)')
db.execute('insert into players values ("name",0,"false",99)')
db.execute('insert into players values ("name",0,"false",100)')

# demonstrate parameterized SQL

# example 1 -- simple placeholders
db.execute('update players set name=?, score=?, active=? where jerseyNum=?', ('Smith, Steve', 42, True, 99))

# example 2 -- named placeholders
db.execute('update players set name=:name, score=:score, active=:active where jerseyNum=:num',
    {'num': 100,
     'name': 'John Doe',
     'active': False,
     'score': -1}
)

# and show the results
for row in db.execute('select * from players'):
   print(row)
