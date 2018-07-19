import mysql.connector
import hashlib

import sys	
import random	

DB_HOST = "localhost"	
DB_USER = "devel"
DB_PASS = "devel"	
DB_NAME = "test"	

def connect_db():	
    ''' Try to connect DB and return DB instance, if not, return False '''	
    try:	
        return mysql.connector.connect(host=DB_HOST, user=DB_USER, passwd=DB_PASS, db=DB_NAME)	
    except:	
        return False	

def create_user(username, passwd):	
    ''' if user was successfully created, returns its ID; returns None on error '''	
    db = connect_db()	
    if not db:	
        print "Can't connect MySQL!"
        return None

    cursor = db.cursor()	

    salt = randomValue(16)	 	
    passwd_md5 = hashlib.md5(salt+passwd).hexdigest()	

    # If username already taken, inform it	
    try:	
        cursor.execute("INSERT INTO users (`username`, `pass_salt`, `pass_md5`) VALUES (%s, %s, %s)", (username, salt, passwd_md5))
        cursor.execute("SELECT userid FROM users WHERE username=%s", (username,) )
        id = cursor.fetchone()
        db.commit()
        cursor.close()
        db.close()
        return id[0]	
    except:	
        print 'Username was already taken. Please select another'	
        return None

def authenticate_user(username, passwd):	
    db = connect_db()	
    if not db:	
        print "Can't connect MySQL!"
        return False

    cursor = db.cursor()	

    cursor.execute("SELECT pass_salt, pass_md5 FROM users WHERE username=%s", (username,))

    row = cursor.fetchone()
    cursor.close()
    db.close()
    if row is None:     # username not found
        return False
    salt = row[0]
    correct_md5 = row[1]
    tried_md5 = hashlib.md5(salt+passwd).hexdigest()
    return correct_md5 == tried_md5

def randomValue(length):	
    ''' Creates random value with given length'''	
    salt_chars = 'abcdefghijklmnopqrstuvwxyz0123456789'

    return ''.join(random.choice(salt_chars) for x in range(length))

if __name__ == '__main__':	
    user = randomValue(10)
    passwd = randomValue(16)	

    new_user_id = create_user(user, passwd)
    if new_user_id is None:
        print 'Failed to create user %s' % user
        sys.exit(1)
    auth = authenticate_user(user, passwd)	
    if auth:	
        print 'User %s authenticated successfully' % user	
    else:	
        print 'User %s failed' % user
