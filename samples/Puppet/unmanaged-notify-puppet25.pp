# Manually manage /tmp/original
# Each puppet run will copy it to /tmp/flag if there's a change and notify
# the exec when it changes.
# 
# The idea here is you might need (in some case) to manually manage a file outside
# of puppet (in this case, "/tmp/original"). Using this example, you can make puppet
# signal other parts of your catalog based on changes to that file.

file {
  # This will, when different, copy /tmp/original to /tmp/flag and notify our
  # exec.
  "/tmp/flag":
    source => "file:///tmp/original",
    notify => Exec["hello world"];
}

exec {
  "hello world":
    command => "/bin/echo hello world",
    refreshonly => true;
}

