
if (!defined($ARGV[1])) {
 	&Usage();
	exit;
}

# First we start m3gdbttd and connect to a crash machine.
# The commands to m3gdbttd cause it to look for stored profile
# information on the machine and write it to two files in the
# current directory. The file "dlinked.syms" contains the symbol
# table and the file "gmon.out" contains the profile counts.

$crash = $ARGV[0];
$kernel = $ARGV[1];

open(DEBUG, "| m3gdbttd $kernel");

print STDOUT "Connect to $crash\n";
print DEBUG "ta t $crash\n";
print DEBUG "domain sweep\n";
print DEBUG "gprof\n";
print DEBUG "det\n";
print DEBUG "q\n";

close(DEBUG);

# Second, we call spinprof which reads the files "dlinked.syms" and
# "gmon.out" and write out the profile information on the standard output.

system("spinprof");

sub Usage {
	print "GraphProfile.perl <crash machine> <kernel>\n";
}
