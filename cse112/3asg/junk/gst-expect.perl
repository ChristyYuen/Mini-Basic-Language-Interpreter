#!/usr/bin/perl
# $Id: gst-expect.perl,v 1.2 2020-09-07 22:53:36-07 - - $

# Expect script which runs gst interactively with data
# from STDIN, showing interactive transacript.

$0 =~ s|.*/||;
use strict;
use warnings;

my $expect = "| expect 2>&1";
open EXPECT, $expect or die "$0: open $expect: $!\n";
print EXPECT "spawn gst\n";

while (my $line = <>) {
   next if $line =~ m/^#!/;
   chomp $line;
   $line =~ s/[\[\]\$"]/\\$&/g;
   print EXPECT "expect \"st> \"\n";
   print EXPECT "send -- \"$line\\n\"\n";
}

close EXPECT;
print "\n";

