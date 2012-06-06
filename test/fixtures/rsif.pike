#! /usr/bin/env pike
// -*- pike -*- $Id$
#pike __REAL_VERSION__

inherit Tools.Standalone.process_files;
string version = ("$Revision$"/" ")[1];
string description = "Replaces strings in files.";
string usage = #"[options] <from> <to> <files>

rsif (\"replace string in file\") replaces all occurrences of the
string <from> with the string <to> in listed files. The name of the
files that were changed are written to stdout. Directories may be
given instead of files, in which case all the files in that directory
will be processed. Available options:
" + default_flag_docs;

int want_args = 2;

string process( string input, string from, string to ) {
  if( has_value( input, from ) )
    return replace( input, from, to );
}
