use MediaWiki::API;
my $api = new MediaWiki::API({api_url => 'http://rosettacode.org/mw/api.php'});

my @pairs =
    sort {$b->[1] <=> $a->[1] or $a->[0] cmp $b->[0]}
    map {$_->{title} =~ s/\ACategory://;
         [$_->{title}, $_->{categoryinfo}{size} || 0];}
    values %{$api->api
      ({action => 'query',
        generator => 'categorymembers',
        gcmtitle => 'Category:Programming Languages',
        gcmlimit => 'max',
        prop => 'categoryinfo'})->{query}{pages}};

for (my $n = 1 ; @pairs ; ++$n)
   {my ($lang, $tasks) = @{shift @pairs};
    printf "%3d. %3d - %s\n", $n, $tasks, $lang;}
