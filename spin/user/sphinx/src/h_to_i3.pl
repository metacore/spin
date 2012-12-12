print "$ARGV[1].\n";
open(IN, $ARGV[1]) || die "$ARGV[1] : $!";
open(OUT, ">$ARGV[2]") || die "$ARGV[2] : $!";
print OUT "INTERFACE $ARGV[0];\n";
print OUT "CONST\n";

while (<IN>) {
    if (/^#define\s+(\w+)\s+(E[A-Z]+)/) {
	;	
    } elsif (/^\#define\s+(\w+)\s+([0-9A-Fa-f]+)/) {
	;
    }  else {
	next;
    }
    ($sym, $val) = ($1, $2);
    if ($val =~ /^0+([0-9]+)/) {
	# octal number 
	$val = "8_$1";
    } elsif ($val =~ /0x(0-9A-F)+/) {
	$val = "16_$1";
    }
    print OUT "  $sym = $val;\n";
}
print OUT "END $ARGV[0].\n";
close IN;
close OUT;
