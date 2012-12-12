#!/uns/bin/perl
use DBTest;

$test_str = "Abort1-Abort2";

$st = test_open("$DBTest::file1", 1);
$tid = test_begin();

test_write($st, 256, $test_str);
test_abort($tid);
test_close($st);
test_end();
check_file("$DBTest::file1", 256, "\0");

