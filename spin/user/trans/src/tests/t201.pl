#!/uns/bin/perl
use DBTest;

print "distributed trans.";

$test_str1 = "Abort1-Abort2";
$test_str2 = "Abort2-Abort1";
$st1 = test_open("foobar");
$st2 = test_open("$DBTest::rem_host:foobar2");
$tid = test_begin();

($r1,$pd1) = test_readx($tid, $st1, 0, 8192);
($r2,$pd2) = test_readx($tid, $st2, 0, 8192);
test_write($r1, 256, $test_str1);
test_write($r2, 256, $test_str2);
test_commit($tid);

test_close($st1);
test_close($st2);
test_crash();

check_file("foobar", 256, $test_str1);
check_file("$DBTest::rem_host/foobar2", 256, $test_str2);
$status;
