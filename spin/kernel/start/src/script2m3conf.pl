$domain = $ARGV[0];
$rc = $ARGV[1];
$target = $ARGV[2];
print STDERR "dom=$domain, rc=$rc.\n";
$rc = "$ENV{HOME}/spin/$rc.rc";

open(RC, $rc) || die "$rc: $!";

while (<RC>) {
    next unless (/^domain addfile (\S+) (\S+)/);
    $object_path = eval("\"$2\"");
    ($dir) = ($object_path =~ m!^(.*)/([^/]*)!);
    push(@OBJS, $object_path);
}
$dir =~ s/~/$ENV{HOME}/;

$dir =~ s/_PROF//; # there is no PROF dir in sal. so use the vanilla one.

print "ObjectPath = \"$dir/\"\n";
print "Objects={}\n";

$idx = 0;
for (@OBJS) {
    ($base, $ext) = ($_ =~ m!([^/.]*)\.(.*)$!);
    print "Objects{\"$idx\"} = [\"$base\", \".$ext\"]\n";
    $idx++;
}
print <<EOD;
ImportedDomains = [
"SpinPublic",
"SpinTrusted"
]
EOD




