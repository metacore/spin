#!/uns/bin/perl
use DBTest;

print STDERR "write across the page boundary.\n";

$test_str = "foo1-foo2-foo3-foo4-foo5" x 10;
$st = test_open("$DBTest::file1");
$tid = test_id("begin");

#test_checkpoint($st);
test_write($st, 256, $test_str);
test_write($st, 8190, $test_str);
test_commit($tid);
test_close($st);
test_crash();

check_file("$DBTest::file1", 256, $test_str);
check_file("$DBTest::file1", 8190, $test_str);
