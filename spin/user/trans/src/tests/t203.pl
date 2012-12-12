#!/uns/bin/perl
use DBTest;

print "distributed trans, big one.";

$test_str1 = "Abort1-Abort2";
$test_str2 = "Abort2-Abort1";
$st1 = test_open("$DBTest::file1", 1);
$st2 = test_open("$DBTest::file3", 1);
$tid = test_begin();

$page = 0;
for (1 .. 20) {
    ($r1,$pd1) = test_readx($tid, $st1, $page, 8192);
    ($r2,$pd2) = test_readx($tid, $st2, $page, 8192);
    test_write($r1, $page + 256, $test_str1);
    test_write($r2, $page + 256, $test_str2);
    test_unpin($st1, $pd1);
    test_unpin($st2, $pd2);	   
    $page += 8192;
}
test_commit($tid);

test_close($st1);
test_close($st2);
test_crash();

$page = 0;
for (1 .. 20) {
    check_file("$DBTest::file1", $page + 256, $test_str1);
    check_file("$DBTest::file3", $page + 256, $test_str2);
    $page += 8192;
}
$status;
