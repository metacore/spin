package DBTest;
require Exporter;
use IPC::Open2;
use POSIX;
use Getopt::Std;
@ISA = qw(Exporter);
@EXPORT = qw(test_restart test_id test_ok check_file 
	     test_read test_readx test_setrange test_checkpoint test_write
	     test_commit test_unpin test_close test_end test_crash test_open
	     test_begin test_abort);



BEGIN {
    getopts('qn:h:1:2:3:');
    if ($opt_q) {
	$quiet = 1;
    }
    $rem_host = $opt_h || "canvas.cs.washington.edu";
    $file1 = $opt_1 || "foo";
    $file2 = $opt_2 || "bar";
    $file3 = $opt_3 || "$rem_host:fofo";
    open(LOG, ">LOG");
    if (-f "./db") {
	$db_path = "./db";
    } elsif (-f "../ALPHA_OSF/db") {
	$db_path = "../ALPHA_OSF/db";
    } elsif (-f "../../ALPHA_OSF/db") {
	$db_path = "../../ALPHA_OSF/db"; 
    } else {
	die "Can't find db executable. Bye.";
    }
    $cmd = "$db_path -q";
    if ($opt_n) {
	$cmd .= " -bufsize $opt_n";
    }
    print "$cmd\n";
    open2(\*RD, \*WR, $cmd);
    select RD; $| = 1;
    select WR; $| = 1;
    select LOG; $| = 1;
    select STDOUT;
}

END {
    close RD;
    close WR;
    close LOG;
}

sub real_path ($) {
    my $path = shift;
    if ($path =~ /^([^:]+):(.*)/) {
	my ($host, $file) = ($1, $2);
	if ($host !~ /\./) {
	    # convert the hostname in to canonical one(i forgot the 
	    # technical jargon for such format...) by adding the domain name
	    # listed in /etc/resolv.conf.
	    open(RESOLVE, "/etc/resolv.conf") || die "/etc/resolv.conf: $!";
	    while (<RESOLVE>) {
		if (/search\s+(\S+)/ || /domain\s+(\S+)/) {
		    $host = $host . "." . $1;
		    last;
		}
	    }
	    close RESOLVE;
	}
	$path = "$host/$file";
    }
    $path;
}
sub test_restart () {
    while (<RD>) {
	chomp;
	print "[$_]\n";
    }
    close RD;
    close WR;
    open2(\*RD, \*WR, "$db_path -q");
    select RD; $| = 1;
    select WR; $| = 1;
    select STDOUT;
    print "test reset\n";
}

sub do_test ($) {
    my ($command) = @_;
    my $resp;
    
    print "sending [$command]\n" if !$quiet;
    print WR $command, "\n";
    print LOG $command, "\n";
    for (;;) {
	chomp($resp = <RD>);
	last if (!$resp);
	return $1 if ($resp =~ /@>>(.*)/);
	print ">$resp\n" if !$quiet;
    }
}
sub test_id ($) {
    my ($command) = @_;
    my $resp;
    $resp = do_test($command);
    if ($resp =~ /^(\d+)\./) {
	print "ok : $command => $resp.\n" if !$quiet;
	return $1;
    }
    print "[$resp]\n" if !$quiet;
    exit 1;
}
sub test_id2 ($) {
    my ($command) = @_;
    my $resp;
    $resp = do_test($command);
    if ($resp =~ /^(\d+),(\d+)\./) {
	print "ok : $command => $resp.\n" if !$quiet;
	return ($1, $2);
    }
    print "[$resp]\n" if !$quiet;
    exit 1;
}
sub test_ok ($) {
    my ($command) = @_;
    my $resp;
    $resp = do_test($command);
    if ($resp =~ /^ok$/) {
	print "ok : $command => $resp.\n" if !$quiet;
	return;
    }
    print "[$resp]\n";
    exit 1;
}
sub test_read ($$$$) {
    test_id2("read $_[0] $_[1] $_[2] $_[3]");
}

sub test_readx ($$$$) {
    test_id2("readx $_[0] $_[1] $_[2] $_[3]");
}

sub test_setrange ($$$$) {
    test_ok("setrange $_[0] $_[1] $_[2] $_[3]");
}
sub test_checkpoint ($) {
    test_ok("checkpoint $_[0]");
}
sub test_write ($$$) {
    test_ok("write $_[0] $_[1] $_[2]");
}
sub test_commit ($) {
    test_ok("commit $_[0]");
}
sub test_abort ($) {
    test_ok("abort $_[0]");
}
sub test_unpin ($$) {
    test_ok("unpin $_[0] $_[1]");
}
sub test_close ($) {
    test_ok("close $_[0]");
}
sub test_end () {
    test_ok("end");
}
sub test_crash () {
    test_ok("crash");
}

# open the FILE. If the 2nd arg is nonnull, the file is
# cleared beforehand.
sub test_open ($;$) {
    my ($file, $remove) = @_;
    if ($remove) {
	my ($path) = real_path($file);
	print STDERR "remove $path.\n";
	unlink($path);
    }
    test_id("open $file");
}
sub test_begin {
    test_id("begin @_");
}

sub check_file ($$$) {
    my ($file, $pos, $content) = @_;
    $file = real_path($file);
    open(IN, $file) || die "$file : $!";
    seek(IN, $pos, SEEK_SET);
    read(IN, $_, length($content));
    if ($_ eq $content	|| ($content =~ /^\0*$/ && $_ eq "")) {
	print "$file : content at $pos ok.\n";
    } else {
	print "$file : content at $pos is $_, but it should be $content.\n";
    }
    close IN;
    $status = 1;
}
1;
