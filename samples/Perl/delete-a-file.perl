use File::Spec::Functions qw(catfile rootdir);
# here
unlink 'input.txt';
rmdir 'docs';
# root dir
unlink catfile rootdir, 'input.txt';
rmdir catfile rootdir, 'docs';
