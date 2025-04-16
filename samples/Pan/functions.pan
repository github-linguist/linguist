################################################################################
# This is 'namespaces/standard/pan/functions.tpl', a pan-templates's file
################################################################################
#
# VERSION:    3.2.7, 21/08/09 22:22
# AUTHOR:     Martin Bock
# MAINTAINER: Example Maintainer <support@example.org>
# LICENSE:    http://cern.ch/eu-datagrid/license.html
#
################################################################################
# Coding style: emulate <TAB> characters with 4 spaces, thanks!
################################################################################
#
# Function definitions
#
################################################################################

declaration template pan/functions;

include 'pan/types';

############################################################
##=
## @function push
## @# push zero or more values onto the end of a list.
##+If the list does not exist or is not defined a new list is
##+created.
## @syntax value:element
## @param:value... the values to push onto list
## @example
##+# "/data" will contain list (1,2,3,4)
##+"/data" = list(1,2);
##+"/data" = push(3,4);
##=
############################################################
function push = {
    # Get the reference to SELF or create an empty list
    # as necessary.
    if (exists(SELF) && is_list(SELF)) {
        v = SELF;
    } else if (!exists(SELF) || !is_defined(SELF)) {
        v = list();
    } else {
        error("push can only be applied to a list");
    };

    # Merge the arguments into the given array.  Neither the
    # first/next or merge functions can be used because the
    # ARGV array cannot be directly referenced.
    i = 0;
    while (i < ARGC) {
        v[length(v)] = ARGV[i];
        i = i + 1;
    };
    v;
};
