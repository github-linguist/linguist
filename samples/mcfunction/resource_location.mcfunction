# test valid
function mypack:foo
function mypack:foo/bar
function mypack:foo/bar/baz
function #mypack:foo
function #mypack:foo/bar
function #mypack:foo/bar/baz

# test valid with with trailing command
setblock ~ ~ ~ mypack:foo destroy
setblock ~ ~ ~ mypack:foo/bar destroy
setblock ~ ~ ~ mypack:foo/bar/baz destroy
setblock ~ ~ ~ #mypack:foo destroy
setblock ~ ~ ~ #mypack:foo/bar destroy
setblock ~ ~ ~ #mypack:foo/bar/baz destroy

# test invalid
function mypack:
function :foo
function #mypack
function #mypack:
function #:foo
function mypack:/
function mypack:foo/
function mypack:/foo
function #mypack:/
function #mypack:foo/
function #mypack:/foo

# test invalid with with trailing command
setblock ~ ~ ~ mypack: destroy
setblock ~ ~ ~ :foo destroy
setblock ~ ~ ~ mypack:/ destroy
setblock ~ ~ ~ mypack:foo/ destroy
setblock ~ ~ ~ mypack:/foo destroy
setblock ~ ~ ~ mypack:/ destroy
setblock ~ ~ ~ mypack:foo/ destroy
setblock ~ ~ ~ mypack:/foo destroy
setblock ~ ~ ~ #mypack:/ destroy
setblock ~ ~ ~ #mypack:foo/ destroy
setblock ~ ~ ~ #mypack:/foo destroy
