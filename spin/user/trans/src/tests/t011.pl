#!/uns/bin/perl
use DBTest;

print "Opening multiple storages.\n";

$test_str = "foo1-foo2-foo3-foo4-foo5" x 10;
$test_str2 = "bar1-bar2-bar3-bar4-bar5" x 10;

$st1 = test_open($DBTest::file1);
$st2 = test_open($DBTest::file2);

$tid = test_begin();

test_write($st1, 256, $test_str);
test_write($st2, 256, $test_str2);

test_commit($tid);
test_crash();

test_restart;
$st1 = test_open($DBTest::file1);
$st2 = test_open($DBTest::file2);
test_close($st1);
test_close($st2);
test_crash;

check_file($DBTest::file1, 256, $test_str);
check_file($DBTest::file2, 256, $test_str2);

