import re

string = "This is a string"

if re.search('string$',string):
    print("Ends with string.")

string = re.sub(" a "," another ",string)
print string
