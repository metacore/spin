#!/uns/bin/perl
use DBTest;

print "Opening multiple storages.\n";

$test_str = "foo1-foo2-foo3-foo4-foo5" x 10;
$test_str2 = "bar1-bar2-bar3-bar4-bar5" x 10;

$st1 = test_open("foobar");
$st2 = test_open("barfoo");

$tid = test_id("begin");

($r1,$pd) = test_readx($tid, $st1, 0, 8192);
test_write($r1, 256, $test_str);

($r2,$pd2) = test_readx($tid, $st2, 0, 8192);
test_write($r2, 256, $test_str2);

test_commit($tid);
test_;

test_restart;
$st1 = test_open("foobar");
$st2 = test_open("barfoo");
test_close($st1);
test_close($st2);
test_end;

check_file("foobar", 256, $test_str);
check_file("barfoo", 256, $test_str2);

