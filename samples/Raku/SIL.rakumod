# This module exports a single subroutine named "SIL" (for Spesh Inline Log).
# When this sub is called, returns a MoarVM::SIL object that can be queried
# for the result of the Spesh Inline Log for the executed program.  Or it
# returns Nil when it is being run inside the program producing the Spesh
# Inline Log.
#
# When introspection of the SIL object is done, the "exit" method should be
# called on the object to prevent the code of the program actually running
# twice.
# 
# A typical use case would be:
#
#  use MoarVM::SIL;
#
#  if SIL() -> $SIL {
#      LEAVE $SIL.exit;
#
#      # do testing, or just show the report:
#      say $SIL.report;
#  }

class BB {
    has Str() $.name is built(:bind);
    has Int() $.id   is built(:bind);
    has Int() $.size is built(:bind);

    method name() {
        $!name ?? "$!name BB($!id)" !! "BB($!id)"
    }

    method gist() { self.Str }
    method Str() {
        $!size ?? "$.name.chop(), $!size bytes)" !! $.name
    }
    method WHICH(--> ValueObjAt:D) {
        ValueObjAt.new("BB|$!id")
    }
}

class Inlined {
    has BB $.inlinee is built(:bind);
    has BB $.into    is built(:bind);

    method gist() { self.Str }
    method Str() { "$!inlinee -> $!into" }
    method WHICH(--> ValueObjAt:D) {
        ValueObjAt.new("BB|$!inlinee.WHICH()|$!into.WHICH()")
    }
}

class Not-Inlined {
    has BB    $.frame  is built(:bind);
    has BB    $.target is built(:bind);
    has Str() $.reason is built(:bind);

    method gist() { self.Str }
    method Str() { "$!frame -> $!target:\n      $!reason" }
    method WHICH(--> ValueObjAt:D) {
        ValueObjAt.new("BB|$!frame.WHICH()|$!target.WHICH()")
    }
}

class MoarVM::SIL {
    has Bag $.inlineds     is built(:bind);
    has Bag $.not-inlineds is built(:bind);
    has Int $status        is built(:bind);

    method sink() { say self.report }
    method report() {
        my str @lines;

        @lines.push: "Spesh Inline Log Report of Process #$*PID ({
            now.DateTime.truncated-to('second')
        })";
        @lines.push: "";

        @lines.push: "Successful inlines";
        @lines.push: "-" x 80;
        @lines.push("{
            $_ == 1 ?? '   ' !! .fmt('%2dx') given $!inlineds{$_}
        } $_.gist()") for $!inlineds.keys.sort: +*.inlinee.id;
        @lines.push: "-" x 80;

        @lines.push: "";
        @lines.push: "Unsuccessful inlines:";
        @lines.push: "-" x 80;
        @lines.push("{
            $_ == 1 ?? '   ' !! .fmt('%2dx') given $!not-inlineds{$_}
        } $_.gist()") for $!not-inlineds.keys.sort: +*.frame.id;
        @lines.push: "-" x 80;

        @lines.join("\n");
    }

    method inlined-by-name($name) {
        $!inlineds.keys.grep: *.name eq $name
    }

    method not-inlined-by-name($name) {
        $!not-inlineds.keys.grep: *.name eq $name
    }

    method gist() { self.report }
    method Str()  { self.report }

    method exit() { exit $!status }
}

sub SIL is export {

    my $proc := Rakudo::Internals.RERUN-WITH(MVM_SPESH_INLINE_LOG => 1);
    return Nil unless $proc;

    my $out := $*OUT;
    my $err := $*ERR;
    my @inlineds;
    my @not-inlineds;
    my $status;

    react {
        whenever $proc.stdout.lines {
            $out.say($_);
        }
        whenever $proc.stderr.lines {
            if m/^ 'Can inline ' (<-[(]>+)
                   '(' (\d+)
                   ') with bytecode size ' (\d+)
                   ' into ' (<-[(]>+)
                   '(' (\d+) /
            {
                @inlineds.push: Inlined.new:
                  inlinee => BB.new(name => $0.chop, id => $1, size => $2),
                  into    => BB.new(name => $3.chop, id => $4);

            }
            elsif m/^ 'Can NOT inline ' (<-[(]>+)
                   '(' (\d+)
                   ') with bytecode size ' (\d+)
                   ' into ' (<-[(]>+)
                   '(' (\d+) 
                   '): ' (.*) /
            {
                @not-inlineds.push: Not-Inlined.new:
                  frame  => BB.new(name => $0.chop, id => $1, size => $2),
                  target => BB.new(name => $3.chop, id => $4),
                  reason => $5;
            }
            else {
                $err.say($_);
            }
        }
        whenever $proc.start(:%*ENV) {
            $status = .exitcode;
        }
    }

    MoarVM::SIL.new(
      inlineds     => @inlineds.Bag,
      not-inlineds => @not-inlineds.Bag,
      status       => $status,
    )
}

# vim: expandtab shiftwidth=4
