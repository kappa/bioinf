#! /usr/bin/perl
use uni::perl;

while (<>) {
	if (/^(\d+) -> ([0-9,]+)$/) {
		my ($from, $to) = ($1, $2);
		say "$from->$_" for split /,/, $to;
	}
	else {
		die;
	}
}
