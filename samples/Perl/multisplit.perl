sub multisplit {
   my ($sep, $string, %opt) = @_ ;
   $sep = join '|', map quotemeta($_), @$sep;
   $sep = "($sep)" if $opt{keep_separators};
   split /$sep/, $string, -1;
}

print "'$_' " for multisplit ['==','!=','='], "a!===b=!=c";
print "\n";
print "'$_' " for multisplit ['==','!=','='], "a!===b=!=c", keep_separators => 1;
print "\n";
