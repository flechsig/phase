#! /usr/bin/perl -w
#  File      : /afs/psi.ch/user/f/flechsig/phase/src/bin/scale-wave-output.pl
#  Date      : <18 Jan 08 14:05:58 flechsig> 
#  Time-stamp: <18 Jan 08 14:30:05 flechsig> 
#  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

#  $Source$ 
#  $Date$
#  $Revision$ 
#  $Author$ 

use strict;
use Getopt::Std;
use vars qw ($opt_h $opt_H  $opt_v $opt_V);

# Initialize our variables
my $version = '0.1';

my $fname= "";
my ($x, $y, $z) = 0.0;


# Main Program -------------------------------------------------------
getopts("hHvV") || die "Command aborted.\n";          
my $verbose = ($opt_v ? $opt_v : 0);
die "Version $version\n" if $opt_V;
&ManPage() if $opt_H;
&Usage()   if $opt_h || @ARGV==0;
$fname= shift;

open (FILE, "$fname") or die "cant open file \"$fname\", $!\n";
$_= <FILE>;
print;
while (<FILE>)
{
    ($x,$y,$z)= split;
    print $x*1e3,"\t",$y*1e3,"\t",$z,"\n"; 
}
close(FILE);
exit();


# Usage --------------------------------------------------------------
# Display the usage message by scanning the POD documentation for the
# usage statement.
sub Usage {
    while (<DATA>) {
	if (/^B<[-A-Za-z0-9_.]+>/) {
	    s/[BI]<([^>]*)>/$1/g;
	    print "Usage: $_";
	    last;
	}
    }
    exit 0;
}

# ManPage ------------------------------------------------------------
# Display the man page by invoking the pod2man (or pod2text) script on
# self.
sub ManPage {
    my($pager) = 'more';
    $pager = $ENV{'PAGER'} if $ENV{'PAGER'};
    if ($ENV{'TERM'} =~ /^(dumb|emacs)$/) {
	system ("pod2text $0");
    } else {
	system ("pod2man $0 | nroff -man | $pager");
    }
    exit 0;
}


__END__

=head1 NAME

scale-wave-output.pl - scale the output of a wave file


=head1 SYNOPSIS

B<scale-wave-output.pl> B<[-v]> B<filename>

=head1 DESCRIPTION 

I<scale-wave-output.pl> - Scale the x,y coordinates of a wave output file, 
wave output in (m)- phase input in (mm). Output on stdout.

=head1 OPTIONS

=over 6

=item B<-h> (help)

Display a usage message.

=item B<-H> (HELP ... man page)

Display the man page.

=item B<-v> (verbose mode)

Display some debug messages. 

=item B<-V> (version)

show version.

=back

=head1 AUTHOR

Uwe Flechsig <uwe.flechsig@psi.ch>

=head1 VERSION

$Revision$

=head1 SEE ALSO


=cut
# end /afs/psi.ch/user/f/flechsig/phase/src/bin/scale-wave-output.pl
