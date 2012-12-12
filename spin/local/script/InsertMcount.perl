
# HISTORY
# 29-May-96  Charles Garrett (garrett) at the University of Washington
#        InsertMcount.perl actually massages
#	 assembly language files to permit profiling of sal code that is
#	 compiled with the DEC C compiler but not linked with their linker.

$command = join(" ", ("/bin/cc -S", @ARGV));
@filepath = split("/", $ARGV[$#ARGV]);
($assembler = $filepath[$#filepath]) =~ s/\.c/\.s/;

# print "$command\n";

system($command);

# print "Munge up $assembler\n";

open(ASSEMBLER, "<$assembler");
open(TMP, ">tmp");

# Are we reading a function declared to be static in the C file.
$static = 0;
$label = "";
$beginning = 0;

# We recognize the beginning of a procedure by ldgp statement which follows
# the .option directive. Profiling instructions are inserted immediately
# after the ldgp.

while (<ASSEMBLER>) {
    if (/\.option/) {
	$beginning = 1;
	print TMP $_;
	print TMP "\t.set\tnoreorder\n";
    } elsif (/ldgp\t\$gp, 0\(\$27\)/) {
	if ($beginning == 1) {
	    print TMP $_;
	    print TMP "\t.set\tnoat\n";
	    print TMP "\tlda\t\$at,_mcount\n";
	    print TMP "\tjsr\t\$at,(\$at),_mcount\n";
	    print TMP "\t.set\tat\n";
	    print TMP "\tldgp\t\$gp,0(\$27)\n";
	    print TMP "\t.set\treorder\n";
	    
	    $beginning = 0;
	}
    } elsif (/\.lab\t/) {
	# Skip label statements, they cause conflicting definitions.
    } elsif (/.ent\t\$\$(\d+)/) {
	# static functions in the C file have their names removed from
	# the .ent declaration. Why? 
	# We try to find the names and restore them in a second pass over
	# the assembly file.
	$static = 1;
	$label = $1;
	print TMP $_;
    } elsif (/.end\t(\S+)/) {
	# Check if this .end declaration contains the name of a static
	# function.
	if ($static) {
	    $static_func_table{$label} = $1;
	}
	$static = 0;
	print TMP $_;
    } else {
	print TMP $_;
    }
}

close(ASSEMBLER);
close(TMP);

open(SECOND_PASS, "<tmp");
open(ASSEMBLER, ">$assembler");

while (<SECOND_PASS>) {
    if (/\$\$(\d+)/) {
	# Wherever a numerical label appears for a static function,
	# replace it with the actual name that we remembered from the
	# first pass.
	if (defined($static_func_table{$1})) {
	    s/\$\$$1/$static_func_table{$1}/g;
	}
    }
    print ASSEMBLER $_;
}

close(ASSEMBLER);
close(SECOND_PASS);			       

@asm_args = @ARGV;
$asm_args[$#asm_args] = $assembler;
$asm_command = join(" ", ("/bin/cc", @asm_args));
# print "$asm_command\n";
system("$asm_command")

