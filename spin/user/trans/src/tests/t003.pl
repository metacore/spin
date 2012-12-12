#!/uns/bin/perl
use DBTest;

print "commit, and crash. no recovery is done.\n";
$test_str = "Commit-Crash";
$st = test_open("$DBTest::file1");
$tid = test_begin();

test_write($st, 256, $test_str);
test_commit($tid);
test_crash();

test_restart();
$st = test_open("$DBTest::file1");
test_close($st);
test_crash();

check_file("$DBTest::file1", 256, $test_str);
