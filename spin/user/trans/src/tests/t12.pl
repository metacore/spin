#!/uns/bin/perl
use DBTest;

print "Opening multiple storages, one storage will abort.\n";

$test_str = "foo1-foo2-foo3-foo4-foo5" x 10;
$test_str2 = "bar1-bar2-bar3-bar4-bar5" x 10;

unlink("foobar");
unlink("Foobar");

$st1 = test_open("foobar");
$st2 = test_open("Foobar");

$tid = test_begin();

$page = 0;
for (1..10) {
    
    ($r1,$pd) = test_readx($tid, $st1, $page, 8192);
    test_write($r1, $page + 256, $test_str);

    ($r2,$pd2) = test_readx($tid, $st2, $page, 8192);
    test_write($r2, $page + 256, $test_str2);

    test_unpin($st1, $pd);
    test_unpin($st2, $pd2);
    $page += 8192;
}

test_commit($tid);
test_end;

test_restart;
$st1 = test_open("foobar");
$st2 = test_open("Foobar");
test_close($st1);
test_close($st2);
test_end;

check_file("foobar", 256, "\0" x length($test_str));
check_file("Foobar", 256, "\0" x length($test_str));

