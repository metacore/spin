#!/uns/bin/perl
use DBTest;

print "lock test. two transactions tries to write to the same region\n";
print "This test will crash. Don't be surprised.\n";

$test_str = "foo1-foo2-foo3-foo4-foo5" x 10;
$st = test_open("foobar");
$tid = test_begin();
$tid2 = test_begin();

($r1,$pd) = test_readx($tid, $st, 0, 8192*2);
test_write($r1, 256, $test_str);

($r2,$pd) = test_readx($tid2, $st, 0, 8192*2);
test_write($r2, 256, $test_str);

# will deadlock.
