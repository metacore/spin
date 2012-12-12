#!/uns/bin/perl

opendir(DIR, ".");
while ($_ = readdir(DIR)) {
    next unless (/^t[05]([0-9][0-9])\.pl$/);
    next if ($1 == 10);
    next if ($1 > 10);
    print "TEST: $_.\n";
    system("$_", "-q", @ARGV);
    if ($? != 0) {
	print "HHHHH!!! $?, $_.\n";
	exit 1;
    }
}
closedir(DIR);

