#!/uns/bin/perl
sub log_open ($) {
    my ($file) = @_;
    open(IN, $file) || die "$file : $!";
}
sub log_close () {
    close IN;
}
$log_file = $ARGV[0] || "trans_log";

sub get_block_size () {
    open(WAL, "../WAL.i3") 
	|| open(WAL, "../src/WAL.i3") 
	    || die "WAL.i3 : $!"; 
    while (<WAL>) {
	if (/BlockSize = ([^;]*);/) {
	    $block_size = eval($1);
	    print "blocksize = $block_size.\n";
	    close WAL;
	    return;
	}
    }
}
@TYPES = qw(Redo Undo
	    S1PCommit SPrepare SCommit SAbort
	    TPrepared TAbort TCommitted
	    CheckPoint ShutDown EOB);
	
sub log_next () {
    my @VAL;
    get_block_size() if ($block_size == 0);
    for (;;) {
	if (eof(IN)) {
	    return wantarray ? () : undef;
	}
	$pos = tell(IN);
	if ($pos % $block_size > ($block_size-24)) {
	    print "seeking to $pos:", (int($pos/$block_size)+1)*$block_size, ".\n";
	    seek(IN, (int($pos/$block_size)+1)*$block_size, 0);
	    $pos = tell(IN);
	}
	$n = read(IN, $in, 24);
	my ($lsn, $x1, $hid, $sid, $type, $size, $ivsum) 
	    = unpack("iiiissi", $in);
	print "type = $type.\n";
	$type_name = $TYPES[$type] || $type;
	@VAL = ($pos, $lsn, $sid, $TYPES[$type], $size);
	if ($sid == 0 || $size > 9000 || $size < 0) {
	    # round up the log.
	    my ($newpos) = ((int(($pos)/$block_size)+1)*$block_size);
	    print("spurious record at $pos (lsn=$lsn,hid=$hid,size=$size,type=$type,sid=$sid): skipping to $newpos\n");
	    seek(IN, $newpos, 0);
	    $pos = tell(IN);
	    print("now at $pos.\n");
	} else {
	    read(IN, $in, $size);
	    last;
	}
    }
    if ($type_name eq 'Redo' || $type_name eq 'Undo') {
	my ($tid, $x1, $prev, $x2, $pos, $x3) = unpack("iiiiii", $in);
	my ($str) = substr($in, 24, 24);
	push(@VAL, $tid, $prev, $pos, $str);
    } elsif ($type_name eq 'Commit' || $type_name eq 'Abort') {
	my ($tid, $x1, $prev, $x2) = unpack("iiii", $in);
	push(@VAL, $tid, $prev);
	#print "tid=$tid, prev=$prev\n";
    } elsif ($type_name eq 'CheckPoint') {
	my ($n_trans, $i) = unpack("ii", $in);
	$in = substr($in, 8);
	my ($trans) = [];
	for $i (1 .. $n_trans) {
	    my ($tid, $x1, $last_lsn, $x2) = unpack("iiii", $in);
	    $in = substr($in, 16);
	    push(@$trans, $tid, $last_lsn);
	}
	push(@VAL, $trans);
	
	my ($pages) = [];
	while (length($in) > 0) {
	    my ($pos, $x1, $recovery_lsn, $x2) = unpack("iiii", $in);
	    push(@{$pages}, $pos, $recovery_lsn);
	    $in = substr($in, 16);
	}
	push(@VAL, $pages);
    } else {
	push(@VAL, "xxx $lsn\n");
    }
    $pos = tell(IN);
    if ($pos % 8) {
	read(IN, $in, 8 - $pos%8);
    }
    $pos = tell(IN);
    return @VAL;
}

log_open($log_file);

while (@VAL = log_next) {
    my ($pos, $lsn, $sid, $type, $size) = @VAL;
    splice(@VAL, 0, 5);

    print "pos=$pos, lsn=$lsn, sid=$sid, type=$type, size=$size, ";
    if ($type eq 'Redo' || $type eq 'Undo') {
	my ($tid, $prev, $pos, $str) = @VAL[0 .. 10];
	printf("tid=$tid, prev=$prev, pos=%lx, str=$str\n", $pos);
    } elsif ($type eq 'Commit' || $type eq 'Abort') {
	my ($tid, $prev) = @VAL[0 .. 10];
	print "tid=$tid, prev=$prev\n";
    } elsif ($type eq 'CheckPoint') {
	my ($trans, $pages) = @VAL[0 .. 5];
	my ($n_trans, $n_pages) = (scalar(@$trans)/2, scalar(@$pages)/2);
	print "trans= ";
	for (0 .. $n_trans-1) {
	    print "(id=$$trans[2*$_], lsn=$$trans[2*$_+1]) ";
	}
	print "\n";
	print "pages= ";
	for (0 .. $n_pages-1) {
	    printf("(pos=0x%lx, lsn=$$pages[2*$_+1]) ", $$pages[2*$_]);
	}
	print "\n";
	
    } else {
	print "\n";
    }
}
