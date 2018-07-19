my $mtime = (stat($file))[9]; # seconds since the epoch

# you should use the more legible version below:
use File::stat qw(stat);
my $mtime = stat($file)->mtime; # seconds since the epoch

utime(stat($file)->atime, time, $file);
# keep atime unchanged
# set mtime to current time
