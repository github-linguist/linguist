py = '''\
 #####    #   #   #####  #    #   ####   #    #
 #    #    # #      #    #    #  #    #  ##   #
 #    #     #       #    ######  #    #  # #  #
 #####      #       #    #    #  #    #  #  # #
 #          #       #    #    #  #    #  #   ##
 #          #       #    #    #   ####   #    #
'''

lines = py.replace('#', '<<<').replace(' ','X').replace('X', '   ').replace('\n', ' Y').replace('< ', '<>').split('Y')
for i, l in enumerate(lines):
    print( '   ' * (len(lines) - i) + l)
