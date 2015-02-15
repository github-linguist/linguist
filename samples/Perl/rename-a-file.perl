use File::Copy qw(move);
use File::Spec::Functions qw(catfile rootdir);
# here
move 'input.txt', 'output.txt';
move 'docs', 'mydocs';
# root dir
move (catfile rootdir, 'input.txt'), (catfile rootdir, 'output.txt');
move (catfile rootdir, 'docs'), (catfile rootdir, 'mydocs');
