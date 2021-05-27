#!/usr/bin/perl
# $Id: modtime.perl,v 1.4 2021-05-22 18:42:27-07 - - $
#
# NAME
#    modtime.perl - print modification time of files
#
# SYNOPSIS
#    modtime.perl filename...
#

$0 =~ s|.*/||;
use POSIX qw(strftime);
use strict;
use warnings;

sub modtime ($) {
   my ($filename) = @_;
   my @stat = stat $filename;
   return @stat ? $stat[9] : undef;
}

for my $filename (@ARGV) {
   my $mtime = modtime $filename;
   if (defined $mtime) {
      my $ctime = strftime "%c", localtime $mtime;
      printf "%-20s %12d %s\n",
             $filename, $mtime, $ctime;
   }else {
      printf "$0: $filename: $!\n";
   }
}

