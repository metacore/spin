
if (!defined($ARGV[1])) {
 	&Usage();
	exit;
}

# Start up m3gdbttd and tell it to write out the flat profile
# information from the target machine. This data will we in 
# ASCII form, but not easy to read. You can pass it to the 
# script "profile.perl" to produce more sensible output.

$crash = $ARGV[0];
$kernel = $ARGV[1];

open(SAVEOUT, ">&STDOUT");
open(STDOUT, ">flat.out");

open(DEBUG, "| m3gdbttd $kernel");

print STDOUT "Connect to $crash\n";
print DEBUG "ta t $crash\n";
print DEBUG "domain sweep\n";
print DEBUG "profile\n";
print DEBUG "det\n";
print DEBUG "q\n";

close(DEBUG);
close(STDOUT);

open(STDOUT, ">&SAVEOUT");

system("profile.perl flat.out");

sub Usage {
	print "FlatProfile.perl <crash machine> <kernel>\n";
}
