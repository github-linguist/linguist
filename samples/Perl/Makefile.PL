use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'Algorithm::Evolutionary::Simple',
    AUTHOR              => 'JJ Merelo <jj@merelo.net>',
    VERSION_FROM        => 'lib/Algorithm/Evolutionary/Simple.pm',
    ABSTRACT_FROM       => 'lib/Algorithm/Evolutionary/Simple.pm',
    LICENSE => 'gpl',
    EXE_FILES => [ 'script/simple-EA.pl',  'script/maxones.pl'],
    PREREQ_PM => {
        'Test::More' => 0,
	'Carp' => 0,
	'Exporter' => 0,
	'Sort::Key::Top' => 0
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'Algorithm-Evolutionary-Simple-*' },
);
