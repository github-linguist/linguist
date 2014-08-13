#!/usr/bin/perl

# v1.0
# nagiostat, program to insert performance-data from Nagios into RRD-archives
# Copyright (C) 2004  Carl Bingel / Svensk IT konsult AB
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

use strict;

## Basic configuration options
my $BASE_DIR = "/usr/share/nagiostat";
my $CONFIG_FILE = "/etc/nagios/nagiostat.conf";  ## Config-file location
my $DEBUG_LOG_FILE = "/var/spool/nagiostat/debug.log";    ## Specify where to create log-file and what filename (must be writable by nagios-user!)
my $DEBUGLEVEL = 1;              ## 0=Nothing, 1=Errors, 2=Warnings, 3=Debug
my $DEBUGOUTPUT = 0;		 ## 0=file, 1=STDERR, 2=STDOUT (for cgi)

require 'shellwords.pl';

## Global vars
my $DEBUG_TIMESTAMP=0;

## Find out how program is run
if( $ARGV[0] eq "-t") {		## -t = test configuration-file
  print STDERR "nagiostat: Testing configuration-file..\n";
  $DEBUGLEVEL=3;
  $DEBUGOUTPUT=1;    ## output errors to console and not file
  my $c = &read_config();
  abort();
} elsif( $ARGV[0] eq "-p") {	## -p = parse performance-data (when started by nagios)
  &parse_perfdata();
} else {
  if( exists $ENV{'GATEWAY_INTERFACE'}) {	## we are run as a CGI-script!
    $DEBUGOUTPUT=2;  ## output errors to web-browser
    &run_as_cgi();
  } else {                                ## print some help-info
    print STDERR "nagiostat: usage:
        -t	Test configuration-file
        -p	Parse/import performance-data (used when called from nagios)
";
  }
}

abort();

sub abort {
   ## logfile: write blank if we wrote anything...
   if( $DEBUG_TIMESTAMP!=0) {
     debug( 1, "");	
   }
   exit;
}

##
##  Program is called as CGI
##
sub run_as_cgi {
   use CGI;
   my $cgi = new CGI;
   
   my $graph_name = $cgi->param( "graph_name");
   my $graph_iteration = $cgi->param( "graph_iteration");

   if( $graph_iteration eq "") {
      print "Content-type: text/html\nExpires: 0\n\n";
   } else {
      print "Content-type: image/gif\nExpires: 0\n\n";
   }

   my $config = read_config();

   if( $graph_name eq "") {      
      ##
      ## display index of graphs
      ##
      display_htmltemplate( $config->{'htmltemplatepath'}."/".$config->{'graphindextemplate'}, $graph_name, $config);
   } else {                      ## display graph
      if( ! exists $config->{'graphs'}->{$graph_name}) {
         debug( 1, "ERROR: Graph '$graph_name' does not exist!");
         exit;
      } elsif( $graph_iteration eq "") {
         ##
         ## Display HTML-page with all the graphs
         ##
         if( ! -r $config->{'htmltemplatepath'}."/".$config->{'graphs'}->{$graph_name}->{'htmltemplate'}) {
            debug( 1, "ERROR: HTML-template '".($config->{'htmltemplatepath'}."/".$config->{'graphs'}->{$graph_name}->{'htmltemplate'})."' is not readable by effective userid!");
            exit;
         }
         display_htmltemplate( $config->{'htmltemplatepath'}."/".$config->{'graphs'}->{$graph_name}->{'htmltemplate'}, $graph_name, $config);
      } else {                   
         ##
         ## generate graph (call 'rrdtool graph')
         ##
         my $rrdtool_cmdline = $config->{'rrdtoolpath'}." graph - ".join( " ", @{$config->{'plottemplates'}->{ $config->{'graphs'}->{$graph_name}->{'plottemplate'} } }); 
         
         ## expand variables
         my $rrdarchive = $config->{'rrdarchivepath'}."/".$config->{'graphs'}->{$graph_name}->{'rrdfilename'};
         $rrdtool_cmdline =~ s/\$f/$rrdarchive/g;
         my $t_start = $config->{'graphtimetemplates'}->{ $config->{'graphs'}->{$graph_name}->{'graphtimetemplate'} }->[$graph_iteration]->{'starttime'};
         $rrdtool_cmdline =~ s/\$s/$t_start/g;
         my $t_end = $config->{'graphtimetemplates'}->{ $config->{'graphs'}->{$graph_name}->{'graphtimetemplate'} }->[$graph_iteration]->{'endtime'};
         $rrdtool_cmdline =~ s/\$e/$t_end/g;
         my $t_descr = $config->{'graphtimetemplates'}->{ $config->{'graphs'}->{$graph_name}->{'graphtimetemplate'} }->[$graph_iteration]->{'description'};
         $rrdtool_cmdline =~ s/\$d/$t_descr/g;

         ## Call rrdtool (should probably be fixed to call it in a better way, like exec)
         print `$rrdtool_cmdline`;
      }

   }

}

## Display HTML template (and do variable-substitution and other stuff)
##
sub display_htmltemplate {
   my( $filename, $graph_name, $config) = @_;

   if( -r $filename) {
      open( HTML, $filename);
      while( <HTML>) {
         ## All is a big regex.. :-)
         s/\$(\w+)/my $t=sub {   
            my $varname = $_[0];
            if( $varname eq "GRAPHNAME") {                           ## return the name of the graph
               if( $config->{'graphs'}->{$graph_name}->{'title'} ne "") {
                  return( $config->{'graphs'}->{$graph_name}->{'title'});
               } else {
                  return( "Graph ".$graph_name);
               }
            } elsif( $varname eq "CURRENTTIME") {                    ## return current date-time
               return( localtime());
            } elsif( $varname eq "GRAPHINDEX" || $varname eq "GRAPHINDEX_ONEROW") {    ## return HTML-code for index of the different graphs
               my $return_html;
               foreach my $gn ( sort keys %{$config->{'graphs'}}) {           
                     $return_html.=(($varname eq "GRAPHINDEX")?"<LI>":"").
                     "<A HREF=\"?graph_name=$gn\">".($config->{'graphs'}->{$gn}->{'title'})."<\/A>".                  # must escape slash since were inside an regex!
                     (($varname eq "GRAPHINDEX_ONEROW")?"&nbsp;&nbsp;":"");
               }
               return( $return_html);
            } elsif( $varname eq "GRAPH_AUTOGENERATE") {             ## return HTML-code for displaying the actual graph-images
               my $iteration_id=0;
               my $return_html;
               foreach my $time ( @{ $config->{'graphtimetemplates'}->{ $config->{'graphs'}->{$graph_name}->{'graphtimetemplate'} } }) {
                 $return_html.="<P>".($time->{'description'})."<BR><IMG SRC=\"?graph_name=$graph_name&graph_iteration=$iteration_id\">"; 
                 $iteration_id++;
               }
               return( $return_html);
            } else {                                                 ## unknown variable
               return( "##UNKNOWN-VARIABLE##");
            }
         }; &$t($1)/eig;   ## i thought that regex would never end!
         print;
      }
      close( HTML);
   } else {
      print "ERROR: HTML-template '$filename' does not exist or is not readable by effective userid.";
   }

}

##
##  Process incoming performance-data (parse output from check-plugin and insert values into rrd-archives)
##
sub parse_perfdata {

  $DEBUG_TIMESTAMP=0;
  
  my $config = read_config();

  my $rrd_updates;

  ##  Provide more symbolic names (same names as the macros in nagios configuration-file)

  my( $LASTCHECK, $HOSTNAME, $SERVICEDESCR, $SERVICESTATE, $OUTPUT, $PERFDATA) = split( /\|!!\|/, $ARGV[1]);
  debug( 3, "**INCOMING PERFDATA:\n  LASTCHECK=$LASTCHECK\n  HOSTNAME=$HOSTNAME\n  SERVICEDESCR=\"$SERVICEDESCR\"\n  SERVICESTATE=\"$SERVICESTATE\"\n  OUTPUT=\"$OUTPUT\"\n  PERFDATA=\"$PERFDATA\"");
  
  my $host_and_descr_found;
  ## Loop through all host_regexes
  foreach my $host_regex ( keys %{$config->{'regexes'}}) {
    ## Loop through all service_description_regexes
    foreach my $service_regex ( keys %{$config->{'regexes'}->{$host_regex}}) {
      if( ($HOSTNAME =~ m/$host_regex/i) && ($SERVICEDESCR =~ m/$service_regex/i) ) {     ## match!
        $host_and_descr_found=1;
        ## Loop through all InsertValue-lines with same host and service_description match
        foreach my $insert_value ( @{$config->{'regexes'}->{$host_regex}->{$service_regex}} ) {
          ## Loop through all regexes that should match values in the output/perfdata
          foreach my $regex ( @{ $config->{'valueregextemplates'}->{$insert_value->{'regextemplate'}} }) {
             my $regex_string = $regex->{'regex'};
             if( $regex->{'regex_what'} eq "output") {         ## do regex on "output"
                if( $OUTPUT =~ m/$regex_string/) {
                   debug( 3, " +VALUE: ".$1);
                   push( @{$rrd_updates->{$insert_value->{'rrdarchive'}}->{'value'}}, $1);
                   push( @{$rrd_updates->{$insert_value->{'rrdarchive'}}->{'dsaname'}}, $regex->{'dsaname'});
                   $rrd_updates->{$insert_value->{'rrdarchive'}}->{'rrdcreatetemplate'} = $insert_value->{'rrdcreatetemplate'};  #$config->{'regexes'}->{$host_regex}->{$service_regex}->[0]->{'rrdcreatetemplate'};
                } else {
                   debug( 2, "**WARNING: No match for value with regex on output '$regex_string'.");
                }
             } else {                                          ## do regex on "perfdata"
                if( $PERFDATA =~ m/$regex_string/) {
                   debug( 3, " +VALUE: ".$1);
                   push( @{$rrd_updates->{$insert_value->{'rrdarchive'}}->{'value'}}, $1);
                   push( @{$rrd_updates->{$insert_value->{'rrdarchive'}}->{'dsaname'}}, $regex->{'dsaname'});
                   $rrd_updates->{$insert_value->{'rrdarchive'}}->{'rrdcreatetemplate'} = $insert_value->{'rrdcreatetemplate'};  #$config->{'regexes'}->{$host_regex}->{$service_regex}->[0]->{'rrdcreatetemplate'};
                } else {
                   debug( 2, "**WARNING: No match for value with regex on perfdata '$regex_string'.");
                }
             }
          }
        }
        
      }
    }
  }
  
  if( !$host_and_descr_found) {
    debug( 2, "**WARNING: Hostname and description didn't match any of the regexes in the config-file.");
  } else {
    ##
    ## Insert the value into the RRD by calling the rrdtool (may be several rrd-archives)
    ##
    foreach my $archive ( keys %{$rrd_updates}) {
      debug( 3, " =INSERT into '$archive': ".join( ",", @{$rrd_updates->{$archive}->{'value'}} )." DSA-names=".join( ",", @{$rrd_updates->{$archive}->{'dsaname'}}) );
      
      my $rrdarchive_filename = $config->{'rrdarchivepath'}."/".$archive;
      
      ## Create RRD-Archive (according to template) if it does not exist
      if( ! -e $rrdarchive_filename) {
         my $rrdtool_cmdline = $config->{'rrdtoolpath'}." create ".$rrdarchive_filename." ".(join( " ", @{$config->{'rrdcreatetemplates'}->{$rrd_updates->{$archive}->{'rrdcreatetemplate'}}}));
         debug( 2, "**CREATING: $rrdarchive_filename, cmdline='".$rrdtool_cmdline."'.");
         `$rrdtool_cmdline`;
      }

      ## Check if rrd-archive is writable
      if( ! -w $rrdarchive_filename) {					## check wheter RRD-archive exists
        debug( 1, "!!ERROR: RRD-archive '".$rrdarchive_filename."' does not exist or is not writable by effective UID."); abort();
      }

      ## Assemle command-line for rrdtool
      my $rrdtool_cmdline = $config->{'rrdtoolpath'}." update ".$rrdarchive_filename.
	                    " --template ".join( ":", @{$rrd_updates->{$archive}->{'dsaname'}}).
                            " $LASTCHECK:".join( ":", @{$rrd_updates->{$archive}->{'value'}});
      debug( 3, " !RRDCMDLINE: ".$rrdtool_cmdline);
      my $result = `$rrdtool_cmdline`;
      if( $result ne "") {
        debug( 1, "!!RESULT: $result");
      }
    }
  }

}




##
##  Read config-file and check for errors
##
sub read_config {
  my $config;
  open( CONFIG, $CONFIG_FILE);
  my( $line_counter);
  while( <CONFIG>) {
    $line_counter++;
    chomp;
    my( @args) = &shellwords( $_);
    my $orig_confline = $_;
    $args[0] = uc( $args[0]);
  
    if( $args[0] eq "INSERTVALUE") {	##  INSERTVALUE-command
      shift @args;
      my( $rrd_filename, $rrdcreatetemplate, $hostname_regex, $servicedescr_regex, $regex_template) = @args;

      if( ! exists $config->{'rrdcreatetemplates'}->{$rrdcreatetemplate}) {
         debug( 1, "!!ERROR: RRDCreateTemplate '$rrdcreatetemplate' is not defined but refered to in line $line_counter."); abort();
      }
      if( $hostname_regex !~ m/^\/(.*)\/$/) {			## verify hostname regex
        debug( 1, "!!ERROR: Hostname regex should be enclosed in slashes, i.e. /HOSTNAME/ and optionally enclosed in quotes if needed. Conf-line $line_counter."); abort();
      } else {
        $hostname_regex = $1;
      }
      if( $servicedescr_regex !~ m/^\/(.*)\/$/) {			## verify service description regex
        debug( 1, "!!ERROR: Service-description regex should be enclosed in slashes, i.e. /SERVICEDESCR/ and optionally enclosed in quotes if needed. Config-line $line_counter.");
        abort();
      } else {
        $servicedescr_regex = $1;
      }
      if( ! exists $config->{'valueregextemplates'}->{$regex_template}) {  ## verify value-regex-template exists
         debug( 1, "!!ERROR: VALUEREGEXTEMPLATE '$regex_template' is not defined on line $line_counter."); abort();
      }
      push( @{$config->{'regexes'}->{$hostname_regex}->{$servicedescr_regex}}, {
            'rrdarchive' => $rrd_filename,
            'rrdcreatetemplate' => $rrdcreatetemplate,
            'regextemplate' => $regex_template
            } );
    } elsif( $args[0] =~ m/^(\s*)#/ || $args[0] eq "") {	## comment or blank row
      ## do nuthin
    } elsif( $args[0] eq "RRDTOOLPATH") {               ## RRDToolPath args: path
      $config->{'rrdtoolpath'} = $args[1];
    } elsif( $args[0] eq "PLOTTEMPLATE") {		## PlotTemplate args: name,htmltemplate,parameters..
      shift @args;
      my( $name, @params) = @args;
      if( $name eq "") {
        debug( 1, "!!ERROR: PLOTTEMPLATE-name must be specified on line $line_counter."); abort();
      }
      if( exists $config->{'plottemplates'}->{$name}) {
        debug( 1, "!!ERROR: PLOTTTEMPLATE-names must be uniqe. Duplicate name found on line: $line_counter."); abort();
      }
      $config->{'plottemplates'}->{$name} = [ @params];
    } elsif( $args[0] eq "GRAPHTIMETEMPLATE") {         ## GraphTimeTemplate args: name,parameters..
      shift @args;
      my( $name, @params) = @args;
      if( $name eq "") {
        debug( 1, "!!ERROR: GRAPHTIMETEMPLATE-name must be specified on line $line_counter."); abort();
      }
      if( exists $config->{'graphtimetemplates'}->{$name}) {
        debug( 1, "!!ERROR: GRAPHTIMETEMPLATE-names must be uniqe. Duplicate name found on line: $line_counter."); abort();
      }
      foreach my $time_template (@params) {
         my( $t_start, $t_end, @t_descr) = split( /:/, $time_template);
         my $t_descr = join( ":", @t_descr);    # workaround if ':' is used in description-string
         if( $t_start eq "" || $t_end eq "") {
            debug( 1, "!!ERROR: GRAPHTIMETEMPLATE, each time-definition should be defined as '<starttime>:<endtime>:<description>' on line $line_counter.");
         }
         push( @{$config->{'graphtimetemplates'}->{$name}}, {
             'starttime' => $t_start,
             'endtime' => $t_end,
             'description' => $t_descr
         });
      }
    } elsif( $args[0] eq "RRDCREATETEMPLATE") {         ## RRDCreateTemplate
      shift @args;
      my( $name, @params) = @args;
      if( $name eq "") {
        debug( 1, "!!ERROR: RRDCREATETEMPLATE-name must be specified on line $line_counter."); abort();
      }
      if( exists $config->{'rrdcreatetemplates'}->{$name}) {
        debug( 1, "!!ERROR: RRDCREATETEMPLATE-names must be uniq. Duplicate name found on line: $line_counter."); abort();
      }
      $config->{'rrdcreatetemplates'}->{$name} = [ @params];
    } elsif( $args[0] eq "VALUEREGEXTEMPLATE") {        ## ValueRegexTemplate
      shift @args;
      my( $template_name, @regexes) = @args;
      if( $template_name eq "") {
         debug( 1, "!!ERROR: VALUEREGEXTEMPLATE-name must be specified on line $line_counter."); abort();
      }
      foreach my $r (@regexes) {
         if( $r !~ m/^(output|perfdata):(\w+):\/(.*)\/$/) {
           debug( 1, "!!ERROR: Value-regex should be formatted as 'output:dsaname:/regex/' or 'perfdata:dsaname:/regex/' depending on in which field to extract the data. The value should be within parantheses in the regex. Config-line $line_counter.");
           abort();
         } else {
           my( $regex_what, $dsa_name, $regex) = ( $1, $2, $3);
           push( @{$config->{'valueregextemplates'}->{$template_name}}, { 
                   'regex_what' => $regex_what, 
                   'regex' => $regex, 
                   'dsaname' => $dsa_name
                  } );
         }
      }
    } elsif( $args[0] eq "RRDARCHIVEPATH") {            ## RRDARCHIVEPATH
      $config->{'rrdarchivepath'} = $args[1];
    } elsif( $args[0] eq "HTMLTEMPLATEPATH") {          ## HTMLTemplatePath
      $config->{'htmltemplatepath'} = $args[1];
    } elsif( $args[0] eq "GRAPHINDEXTEMPLATE") {        ## GraphIndexTemplate
      $config->{'graphindextemplate'} = $args[1];
    } elsif( $args[0] eq "GRAPH") {			## GRAPH args: name,rrdfilename,rrdcreatetemplate,graphtimetemplate,plottemplate,htmltemplate
      if( $args[1] eq "") {
	debug( 1, "!!ERROR: GRAPH-name must be specified on line $line_counter."); abort();
      }
      if( ! exists $config->{'graphtimetemplates'}->{$args[3]}) {
        debug( 1, "!!ERROR: Unknown GRAPHTIMETEMPLATE on line $line_counter."); abort();
      }
      if( ! exists $config->{'plottemplates'}->{$args[4]}) {
        debug( 1, "!!ERROR: Unknown PLOTTEMPLATE on line $line_counter."); abort();
      }
      if( exists $config->{'graphs'}->{$args[1]}) {
        debug( 1, "!!ERROR: Graphnames must be uniqe. Duplicate name found on line: $line_counter.");
        abort();
      } else {
        $config->{'graphs'}->{$args[1]} = {
		'graphname' => $args[1],
		'rrdfilename' => $args[2],
                'graphtimetemplate' => $args[3],
                'plottemplate' => $args[4],
                'htmltemplate' => $args[5],
                'title' => $args[6]
        };
      }
    } else {
      debug( 1, "!!ERROR: Unknown config-file directive on line $line_counter: '".$args[0]."'");
    }
  }
  close( CONFIG);

  if( ! -x $config->{'rrdtoolpath'}) {
    debug( 1, "!!ERROR: RRDTOOLPATH does not point to executable rrdtool-binary.");
    abort();
  }
  if( ! -x $config->{'rrdarchivepath'}) {
     debug( 1, "!!ERROR: RRDARCHIVEPATH, '".($config->{'rrdarchivepath'})."' is not readable by effective userid."); abort();
  }
  if( ! -x $config->{'htmltemplatepath'}) {
     debug( 1, "!!ERROR: HTMLTEMPLATEPATH, '".($config->{'htmltemplatepath'})."' is not readable by effective userid."); abort();
  }

  return( $config);
}


## 
##  Write debug-output/logging
##
sub debug {
  my( $level, $msg) = @_;
  if( $DEBUGLEVEL >= $level) {

    ## write timestamp
    if( !$DEBUG_TIMESTAMP) {	
      $DEBUG_TIMESTAMP=1;
      debug( 1, scalar localtime());
    }

    ## write to file or STDERR
    if( $DEBUGOUTPUT == 0) {
      open( DEBUGOUTPUT, ">>".$DEBUG_LOG_FILE);
      print DEBUGOUTPUT $msg."\n";
      close( DEBUGOUTPUT);
    } elsif( $DEBUGOUTPUT == 2) {
      print "<BR><PRE>$msg</PRE>";
    } else {
      print STDERR $msg."\n";
    }

  }
}
