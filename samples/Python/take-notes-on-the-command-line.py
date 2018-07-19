import sys, datetime, shutil

#sys.argv[1:] = 'go for it'.split()
if len(sys.argv) == 1:
    try:
        f = open('notes.txt', 'r')
        shutil.copyfileobj(f, sys.stdout)
        f.close()
    except IOError:
        pass
else:
    f = open('notes.txt', 'a')
    f.write(datetime.datetime.now().isoformat() + '\n')
    f.write("\t%s\n" % ' '.join(sys.argv[1:]))
    f.close()
