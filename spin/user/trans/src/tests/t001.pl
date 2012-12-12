#!/uns/bin/perl
use DBTest;
print "simple begin-commit-write.\n";
$test_str = "Brown-fox-jumped-over-the-red-dog.";
$st = test_open($DBTest::file1);
$tid = test_id("begin");

test_write($st, 256, $test_str);
test_commit($tid);
test_ok("close $st");
test_ok("end");

check_file($DBTest::file1, 256, $test_str);
exit 0;

