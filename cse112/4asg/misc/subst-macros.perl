#!/usr/bin/perl
# $Id: subst-macros.perl,v 1.1 2021-03-04 15:35:29-08 - - $

# Example substituting macros in a command.

use strict;
use warnings;

my %macro=(
   "FOO"=>'foo',
   "BAR"=>'bar',
);

my $prereq = "hello.c";
my @commands = (
   'echo /${FOO}/${QUX}/${BAR}/',
   'gcc -c $<',
   'echo $$?',
);
print "\nBefore:\n";
map {print "$_\n"} @commands;

map {
   $_ =~ s/\${(.*?)}/$macro{$1}||""/ge;
   $_ =~ s/\$</$prereq/;
   $_ =~ s/\$\$/\$/;
} @commands;

print "\nAfter:\n";
map {print "$_\n"} @commands;

