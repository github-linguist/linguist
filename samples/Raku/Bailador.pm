use Bailador::App;
use Bailador::Request;
use Bailador::Response;
use Bailador::Context;
use HTTP::Easy::PSGI;

module Bailador;

my $app = Bailador::App.current;

our sub import {
    my $file = callframe(1).file;
    my $slash = $file.rindex('/');
    if $slash {
        $app.location = $file.substr(0, $file.rindex('/'));
    } else {
        $app.location = '.';
    }
}

sub route_to_regex($route) {
    $route.split('/').map({
        my $r = $_;
        if $_.substr(0, 1) eq ':' {
            $r = q{(<-[\/\.]>+)};
        }
        $r
    }).join("'/'");
}

multi parse_route(Str $route) {
    my $r = route_to_regex($route);
    return "/ ^ $r \$ /".eval;
}

multi parse_route($route) {
    # do nothing
    $route
}

sub get(Pair $x) is export {
    my $p = parse_route($x.key) => $x.value;
    $app.add_route: 'GET', $p;
    return $x;
}

sub post(Pair $x) is export {
    my $p = parse_route($x.key) => $x.value;
    $app.add_route: 'POST', $p;
    return $x;
}

sub request is export { $app.context.request }

sub content_type(Str $type) is export {
    $app.response.headers<Content-Type> = $type;
}

sub header(Str $name, Cool $value) is export {
    $app.response.headers{$name} = ~$value;
}

sub status(Int $code) is export {
    $app.response.code = $code;
}

sub template(Str $tmpl, *@params) is export {
    $app.template($tmpl, @params);
}

our sub dispatch_request(Bailador::Request $r) {
    return dispatch($r.env);
}

sub dispatch($env) {
    $app.context.env = $env;

    my ($r, $match) = $app.find_route($env);

    if $r {
        status 200;
        if $match {
            $app.response.content = $r.value.(|$match.list);
        } else {
            $app.response.content = $r.value.();
        }
    }

    return $app.response;
}

sub dispatch-psgi($env) {
    return dispatch($env).psgi;
}

sub baile is export {
    given HTTP::Easy::PSGI.new(port => 3000) {
        .app(&dispatch-psgi);
        say "Entering the development dance floor: http://0.0.0.0:3000";
        .run;
    }
}
