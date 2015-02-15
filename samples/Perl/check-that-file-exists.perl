use File::Spec::Functions qw(catfile rootdir);
# here
print -e 'input.txt';
print -d 'docs';
# root dir
print -e catfile rootdir, 'input.txt';
print -d catfile rootdir, 'docs';
