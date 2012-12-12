#!/uns/bin/perl
use DBTest;

print "remote trans.";

$test_str1 = "Abort1-Abort2";
$st1 = test_open("$DBTest::rem_host:foobar2");
$tid = test_begin();

($r1,$pd1) = test_readx($tid, $st1, 0, 8192);
test_write($r1, 256, $test_str1);
test_commit($tid);

test_close($st1);
test_crash();

check_file("$DBTest::rem_host/foobar2", 256, $test_str1);
$status;
