#!/uns/bin/perl
use DBTest;

print "create rather big database file, abort, and crash.\n";
$test_str = "Brown-fox-jumped-over-the-red-dog.";
$st = test_open("foobar", 1);
$tid = test_begin('undo');

$page = 0;
for (1..20) {
    ($r1,$pd) = test_readx($tid, $st, $page, 8192);
    test_write($r1, $page, $test_str);
    test_unpin($st, $pd);
    $page += 8192;
}

#test_abort($tid);
test_crash();
#exit 1;

test_restart();
$storage = test_open("foobar");
test_close($storage);
test_crash();

$page = 0;
for (1..20) {
    check_file("foobar", $page, "\0" x length($test_str));
    $page += 8192;
}

$status;

