import re

# Optional Python 2.x compatibility
#try: input = raw_input
#except: pass

template = '''<name> went for a walk in the park. <he or she>
found a <noun>. <name> decided to take it home.'''

def madlibs(template):
    print('The story template is:\n' + template)
    fields = sorted(set( re.findall('<[^>]+>', template) ))
    values = input('\nInput a comma-separated list of words to replace the following items'
                   '\n  %s: ' % ','.join(fields)).split(',')
    story = template
    for f,v in zip(fields, values):
        story = story.replace(f, v)
    print('\nThe story becomes:\n\n' + story)

madlibs(template)
