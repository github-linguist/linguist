#!/usr/bin/env perl6
use v6;

# This script isn't in bin/ because it's not meant to be installed.

BEGIN say 'Initializing ...';

use Pod::To::HTML;
use URI::Escape;
use lib 'lib';
use Perl6::TypeGraph;
use Perl6::TypeGraph::Viz;
use Perl6::Documentable::Registry;

my $*DEBUG = False;

my $tg;
my %methods-by-type;
my $footer = footer-html;
my $head   = q[
<link rel="icon" href="/favicon.ico" type="favicon.ico" />
<link rel="stylesheet" type="text/css" href="/style.css" media="screen" title="default" />
];


sub url-munge($_) {
    return $_ if m{^ <[a..z]>+ '://'};
    return "/type/$_" if m/^<[A..Z]>/;
    return "/routine/$_" if m/^<[a..z]>/;
    # poor man's <identifier>
    if m/ ^ '&'( \w <[[\w'-]>* ) $/ {
        return "/routine/$0";
    }
    return $_;
}

sub p2h($pod) {
    pod2html($pod, :url(&url-munge), :$footer, :$head);
}

sub pod-gist(Pod::Block $pod, $level = 0) {
    my $leading = ' ' x $level;
    my %confs;
    my @chunks;
    for <config name level caption type> {
        my $thing = $pod.?"$_"();
        if $thing {
            %confs{$_} = $thing ~~ Iterable ?? $thing.perl !! $thing.Str;
        }
    }
    @chunks = $leading, $pod.^name, (%confs.perl if %confs), "\n";
    for $pod.content.list -> $c {
        if $c ~~ Pod::Block {
            @chunks.push: pod-gist($c, $level + 2);
        }
        elsif $c ~~ Str {
            @chunks.push: $c.indent($level + 2), "\n";
        } elsif $c ~~ Positional {
            @chunks.push: $c.map: {
                if $_ ~~ Pod::Block {
                    *.&pod-gist
                } elsif $_ ~~ Str {
                    $_
                }
            }
        }
    }
    @chunks.join;
}

sub recursive-dir($dir) {
    my @todo = $dir;
    gather while @todo {
        my $d = @todo.shift;
        for dir($d) -> $f {
            if $f.f {
                take $f;
            }
            else {
                @todo.push($f.path);
            }
        }
    }
}

sub first-code-block(@pod) {
    if @pod[1] ~~ Pod::Block::Code {
        return @pod[1].content.grep(Str).join;
    }
    '';
}

sub MAIN(Bool :$debug, Bool :$typegraph = False) {
    $*DEBUG = $debug;

    say 'Creating html/ subdirectories ...';
    for '', <type language routine images op op/prefix op/postfix op/infix
             op/circumfix op/postcircumfix op/listop> {
        mkdir "html/$_" unless "html/$_".IO ~~ :e;
    }

    say 'Reading lib/ ...';
    my @source  = recursive-dir('lib').grep(*.f).grep(rx{\.pod$});
       @source .= map: {; .path.subst('lib/', '').subst(rx{\.pod$}, '').subst(:g, '/', '::') => $_ };

    say 'Reading type graph ...';
    $tg = Perl6::TypeGraph.new-from-file('type-graph.txt');
    {
        my %h = $tg.sorted.kv.flat.reverse;
        @source .= sort: { %h{.key} // -1 };
    }

    my $dr = Perl6::Documentable::Registry.new;

    say 'Processing Pod files ...';
    for (0..* Z @source) -> $num, $_ {
        my $podname  = .key;
        my $file     = .value;
        my $what     = $podname ~~ /^<[A..Z]> | '::'/  ?? 'type' !! 'language';
        printf "% 4d/%d: % -40s => %s\n", $num, +@source, $file.path, "$what/$podname";

        my $pod  = eval slurp($file.path) ~ "\n\$=pod";
           $pod .= [0];

        if $what eq 'language' {
            write-language-file(:$dr, :$what, :$pod, :$podname);
        }
        else {
            say pod-gist($pod[0]) if $*DEBUG;
            write-type-file(:$dr, :$what, :pod($pod[0]), :$podname);
        }
    }

    say 'Composing doc registry ...';
    $dr.compose;

    write-disambiguation-files($dr);
    write-op-disambiguation-files($dr);
    write-operator-files($dr);
    write-type-graph-images(:force($typegraph));
    write-search-file($dr);
    write-index-file($dr);

    say 'Writing per-routine files ...';
    my %routine-seen;
    for $dr.lookup('routine', :by<kind>).list -> $d {
        next if %routine-seen{$d.name}++;
        write-routine-file($dr, $d.name);
        print '.'
    }
    say '';

    say 'Processing complete.';
}

sub write-language-file(:$dr, :$what, :$pod, :$podname) {
    spurt "html/$what/$podname.html", p2h($pod);
    if $podname eq 'operators' {
        my @chunks = chunks-grep($pod.content,
                                 :from({ $_ ~~ Pod::Heading and .level == 2}),
                                 :to({  $^b ~~ Pod::Heading and $^b.level <= $^a.level}),
                                );
        for @chunks -> $chunk {
            my $heading = $chunk[0].content[0].content[0];
            next unless $heading ~~ / ^ [in | pre | post | circum | postcircum ] fix | listop /;
            my $what = ~$/;
            my $operator = $heading.split(' ', 2)[1];
            $dr.add-new(
                        :kind<operator>,
                        :subkind($what),
                        :name($operator),
                        :pod($chunk),
                        :!pod-is-complete,
                       );
        }
    }
    $dr.add-new(
                :kind<language>,
                :name($podname),
                :$pod,
                :pod-is-complete,
               );
}

sub write-type-file(:$dr, :$what, :$pod, :$podname) {
    my @chunks = chunks-grep($pod.content,
                             :from({ $_ ~~ Pod::Heading and .level == 2}),
                             :to({  $^b ~~ Pod::Heading and $^b.level <= $^a.level}),
                            );

    if $tg.types{$podname} -> $t {
        $pod.content.push: Pod::Block::Named.new(
            name    => 'Image',
            content => [ "/images/type-graph-$podname.png"],
        );
        $pod.content.push: pod-link(
            'Full-size type graph image as SVG',
            "/images/type-graph-$podname.svg",
        );

        my @mro = $t.mro;
           @mro.shift; # current type is already taken care of

        for $t.roles -> $r {
            next unless %methods-by-type{$r};
            $pod.content.push:
                pod-heading("Methods supplied by role $r"),
                pod-block(
                    "$podname does role ",
                    pod-link($r.name, "/type/$r"),
                    ", which provides the following methods:",
                ),
                %methods-by-type{$r}.list,
                ;
        }
        for @mro -> $c {
            next unless %methods-by-type{$c};
            $pod.content.push:
                pod-heading("Methods supplied by class $c"),
                pod-block(
                    "$podname inherits from class ",
                    pod-link($c.name, "/type/$c"),
                    ", which provides the following methods:",
                ),
                %methods-by-type{$c}.list,
                ;
            for $c.roles -> $r {
                next unless %methods-by-type{$r};
                $pod.content.push:
                    pod-heading("Methods supplied by role $r"),
                    pod-block(
                        "$podname inherits from class ",
                        pod-link($c.name, "/type/$c"),
                        ", which does role ",
                        pod-link($r.name, "/type/$r"),
                        ", which provides the following methods:",
                    ),
                    %methods-by-type{$r}.list,
                    ;
            }
        }
    }
    my $d = $dr.add-new(
        :kind<type>,
        # TODO: subkind
        :$pod,
        :pod-is-complete,
        :name($podname),
    );

    for @chunks -> $chunk {
        my $name = $chunk[0].content[0].content[0];
        say "$podname.$name" if $*DEBUG;
        next if $name ~~ /\s/;
        %methods-by-type{$podname}.push: $chunk;
        # determine whether it's a sub or method
        my Str $subkind;
        {
            my %counter;
            for first-code-block($chunk).lines {
                if ms/^ 'multi'? (sub|method)»/ {
                    %counter{$0}++;
                }
            }
            if %counter == 1 {
                ($subkind,) = %counter.keys;
            }
            if %counter<method> {
                write-qualified-method-call(
                    :$name,
                    :pod($chunk),
                    :type($podname),
                );
            }
        }

        $dr.add-new(
            :kind<routine>,
            :$subkind,
            :$name,
            :pod($chunk),
            :!pod-is-complete,
            :origin($d),
        );
    }

    spurt "html/$what/$podname.html", p2h($pod);
}

sub chunks-grep(:$from!, :&to!, *@elems) {
    my @current;

    gather {
        for @elems -> $c {
            if @current && ($c ~~ $from || to(@current[0], $c)) {
                take [@current];
                @current = ();
                @current.push: $c if $c ~~ $from;
            }
            elsif @current or $c ~~ $from {
                @current.push: $c;
            }
        }
        take [@current] if @current;
    }
}

sub pod-with-title($title, *@blocks) {
    Pod::Block::Named.new(
        name => "pod",
        content => [
            Pod::Block::Named.new(
                name => "TITLE",
                content => Array.new(
                    Pod::Block::Para.new(
                        content => [$title],
                    )
                )
            ),
            @blocks.flat,
        ]
    );
}

sub pod-block(*@content) {
    Pod::Block::Para.new(:@content);
}

sub pod-link($text, $url) {
    Pod::FormattingCode.new(
        type    => 'L',
        content => [
            join('|', $text, $url),
        ],
    );
}

sub pod-item(*@content, :$level = 1) {
    Pod::Item.new(
        :$level,
        :@content,
    );
}

sub pod-heading($name, :$level = 1) {
    Pod::Heading.new(
        :$level,
        :content[pod-block($name)],
    );
}

sub write-type-graph-images(:$force) {
    unless $force {
        my $dest = 'html/images/type-graph-Any.svg'.path;
        if $dest.e && $dest.modified >= 'type-graph.txt'.path.modified {
            say "Not writing type graph images, it seems to be up-to-date";
            say "To force writing of type graph images, supply the --typegraph";
            say "option at the command line, or delete";
            say "file 'html/images/type-graph-Any.svg'";
            return;
        }
    }
    say 'Writing type graph images to html/images/ ...';
    for $tg.sorted -> $type {
        my $viz = Perl6::TypeGraph::Viz.new-for-type($type);
        $viz.to-file("html/images/type-graph-{$type}.svg", format => 'svg');
        $viz.to-file("html/images/type-graph-{$type}.png", format => 'png', size => '8,3');
        print '.'
    }
    say '';

    say 'Writing specialized visualizations to html/images/ ...';
    my %by-group = $tg.sorted.classify(&viz-group);
    %by-group<Exception>.push: $tg.types< Exception Any Mu >;
    %by-group<Metamodel>.push: $tg.types< Any Mu >;

    for %by-group.kv -> $group, @types {
        my $viz = Perl6::TypeGraph::Viz.new(:types(@types),
                                            :dot-hints(viz-hints($group)),
                                            :rank-dir('LR'));
        $viz.to-file("html/images/type-graph-{$group}.svg", format => 'svg');
        $viz.to-file("html/images/type-graph-{$group}.png", format => 'png', size => '8,3');
    }
}

sub viz-group ($type) {
    return 'Metamodel' if $type.name ~~ /^ 'Perl6::Metamodel' /;
    return 'Exception' if $type.name ~~ /^ 'X::' /;
    return 'Any';
}

sub viz-hints ($group) {
    return '' unless $group eq 'Any';

    return '
    subgraph "cluster: Mu children" {
        rank=same;
        style=invis;
        "Any";
        "Junction";
    }
    subgraph "cluster: Pod:: top level" {
        rank=same;
        style=invis;
        "Pod::Config";
        "Pod::Block";
    }
    subgraph "cluster: Date/time handling" {
        rank=same;
        style=invis;
        "Date";
        "DateTime";
        "DateTime-local-timezone";
    }
    subgraph "cluster: Collection roles" {
        rank=same;
        style=invis;
        "Positional";
        "Associative";
        "Baggy";
    }
';
}

sub write-search-file($dr) {
    say 'Writing html/search.html ...';
    my $template = slurp("search_template.html");
    my @items;
    my sub fix-url ($raw) { $raw.substr(1) ~ '.html' };
    @items.push: $dr.lookup('language', :by<kind>).sort(*.name).map({
        "\{ label: \"Language: {.name}\", value: \"{.name}\", url: \"{ fix-url(.url) }\" \}"
    });
    @items.push: $dr.lookup('type', :by<kind>).sort(*.name).map({
        "\{ label: \"Type: {.name}\", value: \"{.name}\", url: \"{ fix-url(.url) }\" \}"
    });
    my %seen;
    @items.push: $dr.lookup('routine', :by<kind>).grep({!%seen{.name}++}).sort(*.name).map({
        "\{ label: \"{ (.subkind // 'Routine').tclc }: {.name}\", value: \"{.name}\", url: \"{ fix-url(.url) }\" \}"
    });
    sub escape(Str $s) {
        $s.trans([</ \\ ">] => [<\\/ \\\\ \\">]);
    }
    @items.push: $dr.lookup('operator', :by<kind>).map({
        qq[\{ label: "$_.human-kind() {escape .name}", value: "{escape .name}", url: "{ fix-url .url }"\}]
    });

    my $items = @items.join(",\n");
    spurt("html/search.html", $template.subst("ITEMS", $items));
}

my %operator_disambiguation_file_written;

sub write-disambiguation-files($dr) {
    say 'Writing disambiguation files ...';
    for $dr.grouped-by('name').kv -> $name, $p is copy {
        print '.';
        my $pod = pod-with-title("Disambiguation for '$name'");
        if $p.elems == 1 {
            $p.=[0] if $p ~~ Array;
            if $p.origin -> $o {
                $pod.content.push:
                    pod-block(
                        pod-link("'$name' is a $p.human-kind()", $p.url),
                        ' from ',
                        pod-link($o.human-kind() ~ ' ' ~ $o.name, $o.url),
                    );
            }
            else {
                $pod.content.push:
                    pod-block(
                        pod-link("'$name' is a $p.human-kind()", $p.url)
                    );
            }
        }
        else {
            $pod.content.push:
                pod-block("'$name' can be anything of the following"),
                $p.map({
                    if .origin -> $o {
                        pod-item(
                            pod-link(.human-kind, .url),
                            ' from ',
                            pod-link($o.human-kind() ~ ' ' ~ $o.name, $o.url),
                        )
                    }
                    else {
                        pod-item( pod-link(.human-kind, .url) )
                    }
                });
        }
        my $html = p2h($pod);
        spurt "html/$name.html", $html;
        if all($p>>.kind) eq 'operator' {
            spurt "html/op/$name.html", $html;
            %operator_disambiguation_file_written{$p[0].name} = 1;
        }
    }
    say '';
}

sub write-op-disambiguation-files($dr) {
    say 'Writing operator disambiguation files ...';
    for $dr.lookup('operator', :by<kind>).classify(*.name).kv -> $name, @ops {
        next unless %operator_disambiguation_file_written{$name};
        my $pod = pod-with-title("Disambiguation for '$name'");
        if @ops == 1 {
            my $p = @ops[0];
            if $p.origin -> $o {
                $pod.content.push:
                    pod-block(
                        pod-link("'$name' is a $p.human-kind()", $p.url),
                        ' from ',
                        pod-link($o.human-kind() ~ ' ' ~ $o.name, $o.url),
                    );
            }
            else {
                $pod.content.push:
                    pod-block(
                        pod-link("'$name' is a $p.human-kind()", $p.url)
                    );
            }
        }
        else {
            $pod.content.push:
                pod-block("'$name' can be anything of the following"),
                @ops.map({
                    if .origin -> $o {
                        pod-item(
                            pod-link(.human-kind, .url),
                            ' from ',
                            pod-link($o.human-kind() ~ ' ' ~ $o.name, $o.url),
                        )
                    }
                    else {
                        pod-item( pod-link(.human-kind, .url) )
                    }
                });
        }
        my $html = p2h($pod);
        spurt "html/$name.html", $html;
    }

}

sub write-operator-files($dr) {
    say 'Writing operator files ...';
    for $dr.lookup('operator', :by<kind>).list -> $doc {
        my $what  = $doc.subkind;
        my $op    = $doc.name;
        my $pod   = pod-with-title(
            "$what.tclc() $op operator",
            pod-block(
                "Documentation for $what $op, extracted from ",
                pod-link("the operators language documentation", "/language/operators")
            ),
            @($doc.pod),
        );
        spurt "html/op/$what/$op.html", p2h($pod);
    }
}

sub write-index-file($dr) {
    say 'Writing html/index.html ...';
    my %routine-seen;
    my $pod = pod-with-title('Perl 6 Documentation',
        Pod::Block::Para.new(
            content => ['Official Perl 6 documentation'],
        ),
        # TODO: add more
        pod-heading("Language Documentation"),
        $dr.lookup('language', :by<kind>).sort(*.name).map({
            pod-item( pod-link(.name, .url) )
        }),
        pod-heading('Types'),
        $dr.lookup('type', :by<kind>).sort(*.name).map({
            pod-item(pod-link(.name, .url))
        }),
        pod-heading('Routines'),
        $dr.lookup('routine', :by<kind>).sort(*.name).map({
            next if %routine-seen{.name}++;
            pod-item(pod-link(.name, .url))
        }),
    );
    spurt 'html/index.html', p2h($pod);
}

sub write-routine-file($dr, $name) {
    say 'Writing html/routine/$name.html ...' if $*DEBUG;
    my @docs = $dr.lookup($name, :by<name>).grep(*.kind eq 'routine');
    my $subkind = 'routine';
    {
        my @subkinds = @docs>>.subkind;
        $subkind = @subkinds[0] if all(@subkinds>>.defined) && [eq] @subkinds;
    }
    my $pod = pod-with-title("Documentation for $subkind $name",
        pod-block("Documentation for $subkind $name, assembled from the
            following types:"),
        @docs.map({
            pod-heading(.origin.name ~ '.' ~ .name),
            pod-block("From ", pod-link(.origin.name, .origin.url ~ '#' ~ .name)),
            .pod.list,
        })
    );
    spurt "html/routine/$name.html", p2h($pod);
}

sub write-qualified-method-call(:$name!, :$pod!, :$type!) {
    my $p = pod-with-title(
        "Documentation for method $type.$name",
        pod-block('From ', pod-link($type, "/type/{$type}#$name")),
        @$pod,
    );
    spurt "html/{$type}.{$name}.html", p2h($p);
}

sub footer-html() {
    state $dt = ~DateTime.now;
    qq[
    <div id="footer">
        <p>
            Generated on $dt from the sources at
            <a href="https://github.com/perl6/doc">perl6/doc on github</a>.
        </p>
        <p>
            This is a work in progress to document Perl 6, and known to be
            incomplete. Your contribution is appreciated.
        </p>
    </div>
    ];
}
