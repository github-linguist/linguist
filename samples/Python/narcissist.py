import sys
with open(sys.argv[0]) as quine:
    code = raw_input("Enter source code: ")
    if code == quine.read():
        print("Accept")
    else:
        print("Reject")
