import string, sys
if sys.version_info[0] < 3:
    input = raw_input

def ispangram(sentence, alphabet=string.ascii_lowercase):
    alphaset = set(alphabet)
    return alphaset <= set(sentence.lower())

print ( ispangram(input('Sentence: ')) )
