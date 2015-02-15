use XUL::Gui;

my $dir = '(.+)(.)';
interval {
    ID(lbl)->value =~ s/$dir/$2$1/;
} 75;

display Label
    id    => 'lbl',
    value => "Hello World! ",
    onclick => sub {toggle $dir => '(.+)(.)', '(.)(.+)'};
