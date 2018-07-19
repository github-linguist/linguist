import __main__, os

def isOnlyInstance():
    # Determine if there are more than the current instance of the application
    # running at the current time.
    return os.system("(( $(ps -ef | grep python | grep '[" +
                     __main__.__file__[0] + "]" + __main__.__file__[1:] +
                     "' | wc -l) > 1 ))") != 0
