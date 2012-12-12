#!/uns/bin/perl
use DBTest;

print "simple begin-abort";
$test_str = "Abort1-Abort2";
$st = test_open("foobar", 1);
$tid = test_begin();

($r1,$pd) = test_readx($tid, $st, 0, 8192);
test_write($r1, 256, $test_str);
test_abort($tid);
test_unpin($st, $pd);
test_close($st);
test_end();

check_file("foobar", 256, "\0"x256);
