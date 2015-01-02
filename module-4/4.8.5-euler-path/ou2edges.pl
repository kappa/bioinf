#! /usr/bin/perl
use uni::perl;

my $path = <>;
my $prev;

for (split /->/, $path) {
	say "$prev->$_" if defined $prev;

	$prev = $_;
}
