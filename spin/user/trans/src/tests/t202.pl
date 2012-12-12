#!/uns/bin/perl
use DBTest;

print "distributed trans, just crash.\n";

$test_str1 = "Abort1-Abort2";
$test_str2 = "Abort2-Abort1";
$st1 = test_open($DBTest::file1, 1);
$st2 = test_open($DBTest::file3, 1);
$tid = test_begin();

($r1,$pd1) = test_readx($tid, $st1, 0, 8192);
($r2,$pd2) = test_readx($tid, $st2, 0, 8192);
test_write($r1, 256, $test_str1);
test_write($r2, 256, $test_str2);
test_crash();

check_file("$DBTest::file1", 256, "\0" x length($test_str1));
check_file("$DBTest::file3", 256, "\0" x length($test_str2));
$status;
