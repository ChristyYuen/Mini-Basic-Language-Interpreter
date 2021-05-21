#!/usr/bin/perl
# $Id: runcmd.perl,v 1.5 2021-02-27 14:26:48-08 - - $
#
# Copy code from this file in order to print an exit status.
#

$0 =~ s|.*/||;
use strict;
use warnings;

my $EXIT_STATUS = 0;
END { exit $EXIT_STATUS; }
sub note (@) { print STDERR @_; };
$SIG{'__WARN__'} = sub { note @_; $EXIT_STATUS = 1; };
$SIG{'__DIE__'} = sub { warn @_; exit; };

# sigtoperl: x86_64 Linux unix1.lt.ucsc.edu
# sigtoperl: Sun Nov 22 17:33:55 2020
my %strsignal = (
    0 => "Unknown signal 0",
    1 => "Hangup",
    2 => "Interrupt",
    3 => "Quit",
    4 => "Illegal instruction",
    5 => "Trace/breakpoint trap",
    6 => "Aborted",
    7 => "Bus error",
    8 => "Floating point exception",
    9 => "Killed",
   10 => "User defined signal 1",
   11 => "Segmentation fault",
   12 => "User defined signal 2",
   13 => "Broken pipe",
   14 => "Alarm clock",
   15 => "Terminated",
   16 => "Stack fault",
   17 => "Child exited",
   18 => "Continued",
   19 => "Stopped (signal)",
   20 => "Stopped",
   21 => "Stopped (tty input)",
   22 => "Stopped (tty output)",
   23 => "Urgent I/O condition",
   24 => "CPU time limit exceeded",
   25 => "File size limit exceeded",
   26 => "Virtual timer expired",
   27 => "Profiling timer expired",
   28 => "Window changed",
   29 => "I/O possible",
   30 => "Power failure",
   31 => "Bad system call",
);

sub run_command (@) {
   my (@command) = @_;
   my $status = eval {no warnings; system @command};
   return undef unless $status;
   return "$!" if $status == -1;
   my $signal = $status & 0x7F;
   my $core = $status & 0x80;
   my $exit = ($status >> 8) & 0xFF;
   return "Error $exit" unless $signal || $core;
   return ($strsignal{$signal} || "Invalid Signal Number $signal")
        . ($core ? " (core dumped)" : "");
}

#
# What you need is the hash and the function.
# The following is just a dummy main function for testing.
#

push @ARGV, "false" unless @ARGV;
print "$0: @ARGV\n";
my $message = run_command "@ARGV";
if ($message) {
   print STDERR "$0: $ARGV[0]: $message\n";
   $EXIT_STATUS = 2;
}

# 
# Wait returns 16 bit number to parent process.
# 
# +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
# |          exit status          | c |          signal           |
# +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
#  15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0
# 
# Bits 15..8 (8-bit field) is the exit status returned by the
# low order 8 bits of the argument to the exit system call or
# returned by the main function.
# 
# Bit 7 is set to 1 if a core file was dumped.
# 
# Bits 6..0 (7-bit field) is the encoded signal number which
# caused the program to crash.
# 
