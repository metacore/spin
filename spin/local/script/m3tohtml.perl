
use Text::Tabs;
use Getopt::Std;

if (!getopts("C:fp")) {
    print STDERR "$0 [-Cdir] [-fp] i3files...\n";
    print STDERR "See also m3tohtml.html.\n";
    exit 1;
}

sub codify ($) {
    my ($x) = @_;
    if ($x ne "") {
	#$x =~ s/  /\&nbsp;\&nbsp;/g;
	$x = "<code>$x</code>";
    } else {
	$x = "";
    }
    $x;
}

#
# Obtain the probable maintainer of the file.
#
sub get_author ($) {
    my ($IN) = @_;
    my (%name, %count);
    while (<$IN>) {
	last if (/HISTORY/);
    }
    while (<$IN>) {
	last if /\*\)/;
	if (/\* \d\d-...-\d\d\s+([^\(]+) \(([^\)]+)\) at/) {
	    $name{$2} = $1;
	    $count{$2}++;
	}
    }
    seek(IN, 0, 0);
    if (scalar(keys %name) < 0) {
	# No HISTORY.
	return "Converted by $ENV{USER}.";
    } else {
	my ($max, $p) = (0);
	# The guy who appears most in the HISTORY is the maintainer :)
	for (keys %name) {
	    if ($count{$_} > $max) {
		$max = $count{$_};
		$p = $_;
	    }
	}
	return "$name{$p} ($p\@cs.washington.edu)";
    }
}

sub format_verb ($) {
    my $text = shift;
    $text =~ s/&/\001/g; # first escape '&'s.
    $text =~ s/</&lt/g;
    $text =~ s/>/&gt/g;
    $text =~ s/\001/&amp/g;
    $text =~ s/\t/        /g;
    #$text =~ s/  /\&nbsp;\&nbsp;/g;
    $text =~ s/  /\&nbsp;\&nbsp;/g;
    "<code>$text</code>";
}

sub protect_escaped_chars ($) {
    my $text = shift;
    $text =~ s/\\\[/\005/g;
    $text =~ s/\\\]/\006/g;
    $text =~ s/\\\"/\007/g;
    $text =~ s/\\</\010/g;
    $text =~ s/\\>/\011/g;
    $text;
}

sub restore_escaped_chars ($) {
    my $text = shift;
    $text =~ s/\005/[/g;
    $text =~ s/\006/]/g;
    $text =~ s/\007/\"/g;
    $text =~ s/\010/&lt;/g;
    $text =~ s/\011/&gt;/g;
    $text;
}

# input is a paragraph of comments.
# output is html text for them.
sub format_comment {
    my ($whole) = @_;
    my (@output_texts);
    my ($para_end_tag) = ("");
    my ($left_margin, @margin_stack, $margin_bias);

    $_[0] =~ /^(\s*)/;
    $margin_bias = length(expand($1));

    for $text (split(/\n/, $whole)) {
	$text = protect_escaped_chars($text);
	$text =~ /^(\s*)(.*)/;
	$left_margin = length(expand($1));

	$left_margin -= $margin_bias if ($left_margin >= $margin_bias);
	$text = $2;

	if ($left_margin > $margin_stack[$#margin_stack]) {
	    push(@output_texts,  "<ul>");
	    push(@margin_stack, $left_margin);
	} elsif ($left_margin < $margin_stack[$#margin_stack]) {
	    while ($margin_stack[$#margin_stack] > $left_margin) {
		pop(@margin_stack);
		push(@output_texts,  "</ul>");
	    }
	}
	#print "text=$text.\n";
	if ($text =~ /XXX(.*)/) {
	    # caveat comments
	    $text = qq(<p><font color="red">$1);
	    $para_end_tag = "</font>$para_end_tag";
	} elsif ($text =~ /Note:(.*)/) {
	    $text = qq(<p><blockquote><strong>Note:</strong><em>$1);
	    $para_end_tag = "</em></blockquote>$para_end_tag";
	} elsif ($text =~ /\*\*\*(.*)/) {
	    # section heading. <h2>
	    if ($section eq "") {
		$section = "1.";
	    } elsif ($section =~ /^(\d+)\./) {
		$section = ($1+1) . ".";
	    }
	    $text = qq(<h2>$section $1</h2>);
	} elsif ($text =~ /\s*\*\*(.*)/) {
	    # section heading <h3>
	    if ($section =~ /^(\d+)\.(\d+)/) {
		$section = $1 . "." . ($2+1);
	    }
	    $text = qq(<h3>$section $1</h3>);
	} elsif ($text eq "") {
	    push(@output_texts, "$para_end_tag<p>");
	    $para_end_tag = "";
	    next;
	}
	if ($text =~ /^\|(.*)/) {
	    # Line beginning with "|"
	    my ($body) = $1 . "";
	    $xx = $body;
	    if ($body =~ /^(.*)`([^\`]*)`(.*)$/) {
		my ($beg, $body, $rest) = ($1, $2, $3);
		$body =~ s!\"([^\"]*)\"!<code>$1</code>!g;
		$text = codify($beg) . "<i>$body</i>" . codify($rest) . "<br>";
	    } else {
		$text = codify($xx) . "<br>";
	    }
	    push(@output_texts, $text);
	    next;
	}

	# nothing to do unless the line contains special letters.
	if ($text !~ /[<\[]/) {
	    $text =~ s!"([^\"]*)"!<code>$1</code>!g;
	    push(@output_texts, $text);
	    next;
	}
	my ($len, $out, $ref_tag,
	    $in_html, $in_str, $in_bracket, $c) = (length($text), "");
	
	for (0 .. $len-1) {   
	    $c = substr($text, $_, 1);
	    if ($in_bracket) {
		if ($c eq "]") {
		    $in_bracket = 0;
		    if ($ref_tag =~ /(.*):(.*)/) {
			$out .= qq(<a="$1">$2</a>);
		    } else {
			$out .= qq(<a="$ref_tag">$ref_tag</a>);
		    }
		} else {
		    $ref_tag .= $c;
		}
	    } elsif ($in_html) {
		if ($c eq ">") {
		    $out .= ">";
		    $in_html = 0;
		} else {
		    $out .= $c;
		}
	    } elsif ($in_str) {
		if ($c eq '"') {
		    $out .= "</code>";
		    $in_str = 0;
		} else {
		    $out .= $c;
		}
	    } else {
		if ($c eq "<") {
		    $out .= "<";
		    $in_html = 1;
		} elsif ($c eq "[") {
		    $in_bracket = 1;
		    $ref_tag = "";
		} elsif ($c eq '"') {
		    $out .= "<code>";
		    $in_str = 1;
		} else {
		    $out .= $c;
		}
	    }
	}
	push(@output_texts, $out);
    }
    if ($para_end_tag ne "") {
	push(@output_texts, $para_end_tag);
    }
    for (1 .. ($#margin_stack)) {
	push(@output_texts, "</ul>");
    }
    
    return(restore_escaped_chars(join("\n", @output_texts)));
}

sub print_prologue ($) {
    my $file = shift;
    my $module = $file;
    $section = "";
    if ($file =~ m!([^./]+).(i3|m3)!) {
	$module = $1;
    }
    print <<EOD
<html>
<head>
<title>$module</title>
<style type="text/css">
    code, pre {font-weight: bold; line-height: 0.8em; }
</style>
</head>
<body>
<h2>$module</h2>

EOD
    ;
}
sub print_epilogue ($) {
    print "</body>\n</html>\n"
}

sub do_file ($) {
    my ($file) = @_;
    if ($file !~ /^(.*).i3$/) {
	die "$file : not an i3 file.";
    }

    open(IN, $file) || die "$file : $!";
    my ($author) = (get_author(IN));

    # The first paragragh is always skipped.
    {
	local($/) = '';
	<IN>;
    }
    @BODY = ();

    # Expand all tabs and read it into @BODY
    while (<IN>) {
	push(@BODY, expand($_));
    }
    close IN;

    print_prologue($file);

    for ($i = 0; $i <= $#BODY; $i++) {
	$_ = $BODY[$i];
	if (/^\s*$/) {
	    # empty line becomes empty line in html.
	    print "<br>\n";
	} elsif (/^\s*\(\*\s*(.*)\*\)/) {
	    # single-line comment
	    print format_comment($1), "<br>\n";
	} elsif (/^(\s*)\(\*\s+(.*)/) {
	    # multiline independent comment
	    my ($comment_start_line) = ($i);
	    my ($text);
	    $i++;
	    while ($BODY[$i] !~ /^(.*)\*\)(.*)/) {
		die "unclosed comment starting at line $comment_start_line" if ($i >= @BODY);
		$text .= $BODY[$i];
		$i++;
	    }
	    $text .= $1;
	    print format_comment($text);
	    print format_comment($2);
	    $BODY[$i] = $2;
	} else {
	    # Program code. 
	    while ($BODY[$i] !~ /^\s*$/) {
		$_ = $BODY[$i];
		if (/^(.*)\(\*\s*(.*)\*\)\s*$/) {
		    # Single line comment.
		    if ($1 ne "") {
			print format_verb($1);
		    }
		    print "<table width=\"100%\"><td><td>", format_comment($2), "</td></table><br>\n";
		} elsif (/^(.*)\(\*(.*)/ && !/\(\*.*\*\)/) {
		    # Multiline comment
		    my ($comment_start_line) = ($i);
		    print "<table width=\"100%\"><tr>\n";
		    if ($1 ne "") {
			print "<td>", format_verb($1);
		    }
		    my ($text) = ($2);
		    $i++; 
		    while ($BODY[$i] !~ /^(.*)\*\)(.*)/) {
			die "unclosed comment starting at line $comment_start_line" if ($i >= @BODY);
			$text .= $BODY[$i];
			$i++;
		    }
		    $BODY[$i] =~ /^(.*)\*\)(.*)/;
		    $text .= $1;
		    print "<td>", format_comment($text), "</table><br>\n";
		    $BODY[$i] = $2;
		    $i--;
		} else {
		    print format_verb($BODY[$i]), "<br>";
		}
		$i++;
	    }
	}
    }
    print "<hr>\n";
    print "Last updated: ", `date`, "<br>\n";
    print "<address>$author</address>\n";
    print_epilogue($file);
}

$user = $ENV{USER};
$tmp = ("/tmp/foo$user");

sub timestamp ($) {
    my $file = shift;
    return 0 unless (-e $file);
    return (stat($file))[9];
}

$output_dir = $opt_C || ".";
if (!-d $output_dir) {
    mkdir($output_dir, 0755) || die "$output_dir: $!";
}

if (!$opt_p) {
    for (@ARGV) {
	my ($html_file);
	if ($_ =~ m!([^./]+).(i3|m3)!) {
	    $html_file = "$output_dir/$1.html";
	} else {
	    $html_file = "$output_dir/$_.html";
	}
	print STDOUT "$_ -> $html_file\n";
	open(OUT, ">$html_file") || die "$html_file: $!";
	select OUT;
	do_file($_);
	close OUT;
    }
} else {
    # when only one file is given, it is processed and put to stdout.
    $_ = $ARGV[0];
    do_file($_);
}



