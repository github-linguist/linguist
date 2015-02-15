#!/usr/bin/perl -n
END{ print $all }

substr($_, length($l)) and $all = $l = $_
	or substr($l, length) or $all .= $_;
