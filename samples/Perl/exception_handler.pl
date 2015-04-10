package exception_handler;
use sigtrap qw(die normal-signals);
use IO::Handle;
use Carp;
use File::Spec;
use File::Basename;
use Data::Dumper;

use sigtrap 'handler', \&tm_die;

$Carp::CarpLevel = 1;     # How many extra package levels to skip on carp.

BEGIN {
    *CORE::GLOBAL::die = \&tm_die;
    $main::SIG{__DIE__} = \&tm_die;
    my $error_fd = $ENV{"TM_ERROR_FD"};
	open (TM_ERROR_FD, ">&=$error_fd");
	TM_ERROR_FD->autoflush(1);
}

sub realwarn { CORE::warn(@_); }
sub realdie { CORE::die(@_); }

sub longmess {
    my ($arg, @rest) = shift;
    {
        local $@;
        # XXX fix require to not clear $@?
        # don't use require unless we need to (for Safe compartments)
        require Carp::Heavy unless $INC{"Carp/Heavy.pm"};
    }
    # Icky backwards compatibility wrapper. :-(
    my $call_pack = caller();
    if ($Internal{$call_pack} or $Carp::CarpInternal{$call_pack}) {
      return longmess_heavy($arg, @rest);
    }
    else {
      local $Carp::CarpLevel = $Carp::CarpLevel + 1;
      return longmess_heavy($arg, @rest);
    }
}

sub longmess_heavy {
  return @_ if ref($_[0]); # don't break references as exceptions
  my $i = Carp::long_error_loc();
  my ($arg, @rest) = @_;
  return ret_backtrace($i, $arg, @rest);
}

sub quote {
	my $str = shift;
	$str =~ s/([^A-Za-z0-9\/_.-])/sprintf("%%%02X", ord($1))/seg;
	return $str;
}

sub url_and_display_name {
	my $file = shift;
	my $url = "";
	my $display_name = "";
	$display_name = basename($file);
	$url = 'url=file://' . quote($file);
	return ($url, $display_name);
}

# Returns a full stack backtrace starting from where it is
# told.
sub ret_backtrace { 
  my ($i, $arg, @rest) = @_;
  my $mess;
  $i++;

  my $tid_msg = '';
  if (defined &Thread::tid) {
    my $tid = Thread->self->tid;
    $tid_msg = " thread $tid" if $tid;
  }

  my %i = Carp::caller_info($i);
  $arg =~ s/\n/\<br\>/g;
  $i{sub} =~ s/tm_die/die/g;
  $mess .= "<div id='exception_report' class='framed'>\n";
  $mess .= "<p id='exception'><strong>$arg</strong></p>\n";
  $mess .= "<blockquote><table border='0' cellspacing='0' cellpadding='0'>\n";
  my ($url, $display_name) = url_and_display_name($i{file});
  $mess .= "<tr><td><a href='txmt://open?line=$i{line}&" . $url . "'>$i{sub}</a></td><td>&nbsp;in $display_name at line $i{line}$tid_msg</td></tr>\n";
  while (my %i = Carp::caller_info(++$i)) {
      ($url, $display_name) = url_and_display_name($i{file});
      $mess .= "<tr><td><a href='txmt://open?line=$i{line}&" . $url . "'>$i{sub}</a></td><td>&nbsp;in $display_name at line $i{line}$tid_msg</td></tr>\n";
  }
  $mess .= "</table></blockquote></div>";
  return $mess;
}

sub ineval {
  (exists $ENV{MOD_PERL} ? 0 : $^S) || Carp::longmess() =~ /eval [\{\']/m
}

sub htmlize {
	my $l = shift;
	$l =~ s/&/&amp;/g;
	$l =~ s/</&lt;/g;
	$l =~ s/>/&gt;/g;
	return $l;
}

sub tm_die {
  my ($arg,@rest) = @_;
  if (ineval()) {
      realdie ($arg,@rest) if ineval();
  }
  if (!ref($arg)) {
    print TM_ERROR_FD longmess($arg,@rest);
  }
  exit($!);
}

1;
