#!/usr/bin/perl
# $Id: quotemeta.perl,v 1.5 2021-03-05 19:38:47-08 - - $

@patterns = ('%.o', '%.cmi');

for $pat (@patterns) {
   my $regex = quotemeta $pat;
   $regex =~ s/\\%/(.*)/;
   $regex = "^$regex\$";
   $regexhash{$pat} = $regex;
};

for $key (keys %regexhash) {
   print "'$key' => '$regexhash{$key}'\n";
}

$want = "foobar.o";

print "\nsearching '$want'\n";
for $pat (keys %regexhash) {
   my $regex = $regexhash{$pat};
   if ($want =~ $regex) {
      my $percent = $1;
      print "'$want' matches '$regex' and % is $percent\n";
   }
}

