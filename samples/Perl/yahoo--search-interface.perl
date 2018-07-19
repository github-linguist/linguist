package YahooSearch;

use Encode;
use HTTP::Cookies;
use WWW::Mechanize;

# --- Internals -------------------------------------------------

sub apply (&$)
 {my $f = shift; local $_ = shift; $f->(); return $_;}

# We construct a cookie to get 100 results per page and prevent
# "enhanced results".
my $search_prefs = 'v=1&n=100&sm=' .
    apply {s/([^a-zA-Z0-9])/sprintf '%%%02X', ord $1/ge}
    join '|',
    map {'!' . $_}
    qw(hsb Zq0 XbM sss dDO VFM RQh uZ0 Fxe yCl GP4 FZK yNC mEG niH);
my $cookies = HTTP::Cookies->new;
$cookies->set_cookie(0, 'sB', $search_prefs, '/', 'search.yahoo.com');

my $mech = new WWW::Mechanize
   (cookie_jar => $cookies,
    stack_depth => 0);

sub read_page
 {my ($next, $page, @results) =
     ($mech->find_link(text => 'Next >')->url,
      decode 'iso-8859-1', $mech->content);
  while ($page =~ m
         {<h3> <a \s class="yschttl \s spt" \s
          href=" ([^"]+) " \s* >                #"
          (.+?) </a>
          .+?
          <div \s class="abstr">
          (.+?) </div>}xg)
     {push @results, {url => $1, title => $2, content => $3};
      foreach ( @{$results[-1]}{qw(title content)} )
         {s/<.+?>//g;
          $_ = encode 'utf8', $_;}}
  return $next, \@results;}

# --- Methods ---------------------------------------------------

sub new
 {my $invocant = shift;
  my $class = ref($invocant) || $invocant;
  $mech->get('http://search.yahoo.com/search?p=' . apply
     {s/([^a-zA-Z0-9 ])/sprintf '%%%02X', ord $1/ge;
      s/ /+/g;}
    shift);
  my ($next, $results) = read_page();
  return bless {link_to_next => $next, results => $results}, $class;}

sub results
 {@{shift()->{results}};}

sub next_page
 {my $invocant = shift;
  my $next = $invocant->{link_to_next};
  unless ($next)
     {$invocant->{results} = [];
      return undef;}
  $mech->get($next);
  ($next, my $results) = read_page();
  $invocant->{link_to_next} = $next;
  $invocant->{results} = $results;
  return 1;}
