use feature "switch";
given ($input) {
    when (0)          { print 'input == 0'; }
    when ('coffee')   { print 'input equal coffee'; }
    when ([1..9])     { print 'input between 1 and 9'; }
    when (/rats/)     { print 'input matches rats'; }
    default           { do_fallback; }
}
