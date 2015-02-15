use File::Spec::Functions qw(catfile rootdir);
print -s 'input.txt';
print -s catfile rootdir, 'input.txt';
