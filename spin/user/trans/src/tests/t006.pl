#!/uns/bin/perl
use DBTest;

print STDERR "create rather big database file.";

$test_str = "Brown-fox-jumped-over-the-red-dog." x 10;
$st = test_open("$DBTest::file1");
$tid = test_begin();

$page = 0;
for (1..10) {
    test_write($st, $page, $test_str);
    $page += 8192;
}

test_commit($tid);
test_close($st);
test_crash();

$page = 0;
for (1..10) {
    check_file("$DBTest::file1", $page, $test_str);
    $page += 8192;
}
