#!/uns/bin/perl
use DBTest;

# checkpoint, commit, and crash, and then recover.

$test_str = "Commit-Crash";
$st = test_open("$DBTest::file1");

$tid = test_begin();
test_ok("checkpoint $st");
test_write($st, 256, $test_str);
test_commit($tid);

$tid = test_begin();
test_write($st, 512, $test_str);
test_abort($tid);
test_crash();

test_restart();
$st = test_open("$DBTest::file1");
test_close($st);
test_end();

check_file("$DBTest::file1", 256, $test_str);
check_file("$DBTest::file1", 512, "\0" x length($test_str));
