#! /usr/local/bin/perl -ws

use Lingua::EN::Inflect qw { classical PL };
use vars qw { $classical $modern };

classical if $classical && !$modern;

print "singular> one ";
while (<>)
{
	chomp;
	exit if /^\.$/;
	if (/^\-classical$/)	{ classical ; print "[going classical]"}
	elsif (/^-modern$/)	{ classical 0; print "[going modern]" }
	else			{ print "   plural> two ", PL $_ }
	print "\nsingular> one ";
}
