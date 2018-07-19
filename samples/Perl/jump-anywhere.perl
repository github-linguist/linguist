sub outer {
    print "In outer, calling inner:\n";
    inner();
  OUTER:
    print "at label OUTER\n";
}

sub inner {
    print "In inner\n";

    goto SKIP;      # goto same block level
    print "This should be skipped\n";
  SKIP:
    print "at label SKIP\n";

    goto OUTER;     # goto whatever OUTER label there is on frame stack.
                    # if there isn't any, exception will be raised
    print "Inner should never reach here\n";
}

sub disguise {
    goto &outer;    # a different type of goto, it replaces the stack frame
                    # with the outer() function's and pretend we called
                    # that function to begin with
    print "Can't reach this statement\n";
}

print "Calling outer:\n";
outer();

print "\nCalling disguise:\n";
disguise();

print "\nCalling inner:\n";
inner();                # will die
