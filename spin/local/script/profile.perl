
# HISTORY
# 17-Apr-96  Charles Garrett (garrett) at the University of Washington
#	This script can be used to massage the output of the m3gdbttd
#	"profile" command. Currently, we combine all of the samples
#	for a single procedure and print out the procedures in
#	decreasing order of frequency. We also display separately
#	the samples which were affected by clock interrupt masking
#	and those which were not.
#


while (<>) {
    if (/0x\S+ <([^\+]+)\+?(\d*)>\s*(\d+)/) {
		$frequency{$1} += $3;
		if ($2 % 2 == 0) {
			$notDelayed{$1} += $3;
			$notDelayedTotal += $3;
	    } else {
			$delayed{$1} += $3;
			$delayedTotal += $3;
		}
	    $totalHits += $3;
	}
}

print "All Samples\n";
print "Count\tPercent\t\tProcedure\n-----\t-------\t\t---------\n";
foreach $k (sort decreasing keys(%frequency)) {
	printf("$frequency{$k}\t%.2f\t\t$k\n", 100 * $frequency{$k}/$totalHits);
}

print "\nSamples at low interrupt level\n";
print "Count\tPercent\t\tProcedure\n-----\t-------\t\t---------\n";
foreach $k (sort decreasingNotDelayed keys(%notDelayed)) {
	printf("$notDelayed{$k}\t%.2f\t\t$k\n", 
	       100 * $notDelayed{$k}/$notDelayedTotal);
}

print "\nSamples at high interrupt level\n";
print "Count\tPercent\t\tProcedure\n-----\t-------\t\t---------\n";
foreach $k (sort decreasingDelayed keys(%delayed)) {
	printf("$delayed{$k}\t%.2f\t\t$k\n", 
	       100 * $delayed{$k}/$delayedTotal);
}


sub decreasing {
	$frequency{$b} <=> $frequency{$a};
}

sub decreasingDelayed {
	$delayed{$b} <=> $delayed{$a};
}

sub decreasingNotDelayed {
	$notDelayed{$b} <=> $notDelayed{$a};
}

