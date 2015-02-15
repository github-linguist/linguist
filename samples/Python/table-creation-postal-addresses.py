>>> import sqlite3
>>> conn = sqlite3.connect(':memory:')
>>> conn.execute('''CREATE TABLE address (
	addrID		INTEGER PRIMARY KEY AUTOINCREMENT,
	addrStreet	TEXT NOT NULL,
	addrCity	TEXT NOT NULL,
	addrState	TEXT NOT NULL,
	addrZIP		TEXT NOT NULL
    )''')
<sqlite3.Cursor object at 0x013265C0>
>>>
