#! /usr/local/bin/perl -ws

use Lingua::EN::Inflect qw { classical NO };
use vars qw { $classical $modern };

classical if $classical && !$modern;

print "count word> ";
while (<>)
{
	chomp;
	exit if /^\.$/;
	if (/^\-classical$/)	{ classical ; print "[going classical]"}
	elsif (/^-modern$/)	{ classical 0; print "[going modern]" }
	else			
	{
		/\s*([^\s]+)\s+(.*)/ or next;
		print "            ", NO($2,$1), "\n";
	}
	print "\ncount word> ";
}
