#!/usr/bin/perl
# Open the browser to navigate to the page of a Github commit hash.
#
# Usage: git remote -v | open-github-commit.pl <commit hash id>

if (@ARGV < 1) {
    print "Usage: $0 <github commit hash>\n";
    exit 1;
}

my $commit_id = $ARGV[0];

foreach my $line (<STDIN>) {
    chomp($line);
    if ($line =~ /^origin\s+(?:\w+@)github.com:([^\s]+?)(?:\.git)? \(fetch\)/) {
        exec "open https://github.com/$1/commit/$commit_id\n";
    }
}
