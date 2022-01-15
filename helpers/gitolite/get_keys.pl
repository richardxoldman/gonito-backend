#!/usr/bin/perl

use strict;

use DBI;

my $dbh = DBI->connect("dbi:Pg:dbname=gonito", "", "");

my $sh = $dbh->prepare(qq{SELECT * FROM "user" U, public_key K WHERE U.id = K.user and U.id >= 40});

$sh->execute();

while (my $key = $sh->fetchrow_hashref()) {
    my $local_id = $key->{'local_id'};
    my $pkey = $key->{'pubkey'};

    if ($pkey =~ /PRIVATE/) {
	print STDERR "$local_id has private key!\n";
    } elsif (! defined($local_id) && $local_id !~ /\S/) {
	print STDERR "not defined local_id\n";
    } else {
	if ($pkey !~ /^ssh-/) {
	    $pkey = 'ssh-rsa ' . $pkey;
	}

	open my $fh, '>', $local_id.".pub";
	print $fh $pkey;
	close $fh;
    }
}
