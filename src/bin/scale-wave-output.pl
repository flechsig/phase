#! /usr/bin/perl -w
#  File      : /afs/psi.ch/user/f/flechsig/phase/src/bin/scale-wave-output.pl
#  Date      : <18 Jan 08 14:05:58 flechsig> 
#  Time-stamp: <18 Jan 08 14:06:20 flechsig> 
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

my ($ch, $shape, $fname, $i, $j, $aij, @coeff, $r, $rho, $l, $a, $tmp) = ();
my ($input, $today)= ();

# Main Program -------------------------------------------------------
getopts("hHvV") || die "Command aborted.\n";          
my $verbose = ($opt_v ? $opt_v : 0);
die "Version $version\n" if $opt_V;
&ManPage() if $opt_H;
&Usage()   if $opt_h || @ARGV==0;
$fname= shift;
$today= localtime;

print "\n", "=" x 80, "\n";
print "  create coefficient file for an optical element for PHASE\n";
print "=" x 80, "\n";

if (-e $fname)
{
   print "\nfile \"$fname\" exists: do you really want to overwrite? (y/n) ";
   $ch= <STDIN>;
   chomp $ch;
   die "we do not overwrite \"$fname\"- exit\n" unless $ch eq 'y';
}
open (FILE, "> $fname") or die "cant open file \"$fname\", $!\n";
print "input the shape of the element: (c)one, (p)lane, (t)oroid: ";
$ch= <STDIN>;
chomp $ch;

die "$ch - unknown shape, exit" unless grep { $ch eq $_ } ("t", "p", "c"); 
&torus if $ch eq "t";
&plane if $ch eq "p";
&torus if $ch eq "c";

$input=~ s/\n//g;

# ab hier wird das File geschrieben, die Parameter wurden mit push in
# einer Liste zwischengespeichert und mit splice wieder ausgelesen
print FILE << "END";
#######################################################################
# PHASE coefficient file 
# created by create-coefficients.pl  $today
# format: i j a(i,j)
# shape : $shape
# input : $input
####################################################################### 
END

    print "\ncalculated coefficients (a(i,j) != 0):\n" if $verbose;
while (($i,$j,$aij)= splice (@coeff, 0, 3))
{
    print FILE "$i $j $aij\n";
    print "$i $j $aij\n" if $verbose;
}
print "\n" if $verbose;
print FILE "# end $fname\n";
close(FILE);
print "file \"$fname\" generated\n";

# end main

#
# torus
#
sub torus
{
    $shape= "toroid";
    print "toroid";
    print "(short | long) radius= 0 means plane, radius < 0 means konvex\n";
    print "input: long radius (mm): ";
    $r= <STDIN>;  
    print "input: short radius (mm): ";
    $rho= <STDIN>;  
    if (abs($rho) > 0)
    {
       $tmp= 0.5/ $rho;
       push (@coeff, 0, 2, $tmp);
       $tmp= 1.0/ (8.0* $rho**3);
       push (@coeff, 0, 4, $tmp);
    }
    if (abs($r) > 0)
    {
       $tmp= 0.5/ $r;
       push (@coeff, 2, 0, $tmp);
       $tmp= 1.0/ (8.0* $r**3);
       push (@coeff, 4, 0, $tmp);
       if (abs($rho) > 0)
       {
	   $tmp= 1.0/ (4.0* $r**2 * $rho);
	   push (@coeff, 2, 2, $tmp);
       }
    }
    $input= "R= $r mm, rho= $rho mm";
} # end torus

#
# plane
#
sub plane
{
    $shape= "plane";
    print "flat mirror";
# write one coefficient
    push (@coeff, 0, 0, 0.0);
    $input= "no input required";
} # end plane


#
# cone
#
sub cone
{
    $shape= "cone";
    print "cone";
    print "(downstream | upstream) radius (R, rho) = 0 means plane, \n";
    print "(r, rho) < 0 means konvex\n";
    print "input: upstream radius rho (mm): ";
    $rho= <STDIN>;  
    print "input: downstream radius R (mm): ";
    $r= <STDIN>; 
    print "input: length (mm): ";
    $l= <STDIN>; 
    die "exit: l < 0 not allowed!\n" if ($l < 0);
    die "exit: R and rho must have the same sign!\n" if (($r * $rho) < 0);
    $tmp= 1/ ($r + $rho);
    push (@coeff, 0, 2, $tmp);   # l^2
    $tmp= 1/ ($r + $rho)**3;     # l^4
    push (@coeff, 0, 4, $tmp);
    $tmp= -2 *($r- $rho)/($l* ($r + $rho)**2); # l^2 w
    push (@coeff, 1, 2, $tmp);
    $tmp= -6 *($r- $rho)/($l* ($r + $rho)**4); # l^4 w
    push (@coeff, 1, 4, $tmp);
    $tmp= -4 *($r- $rho)**2/($l**2 * ($r + $rho)**3); # l^2 w^2
    push (@coeff, 2, 2, $tmp);
    $tmp= -8 *($r- $rho)**3/($l**3 * ($r + $rho)**4); # l^2 w^3
    push (@coeff, 3, 2, $tmp);
    $input= "R= $r mm, rho= $rho mm, l= $l mm";


} # end cone


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

create-coefficients.pl - create a coefficient file for PHASE


=head1 SYNOPSIS

B<create-coefficients.pl> B<[-v]> B<filename>

=head1 DESCRIPTION 

I<create-coefficients.pl> Create a coefficient file of an optical 
element to be used with PHASE. So far toroidal and conical shapes 
are implemented.

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

Uwe Flechsig <flechsig@psi.ch>

=head1 VERSION

$Revision$

=head1 SEE ALSO


=cut
# end /home/pss060/sls/flechsig/phase/utils/create-coefficients.pl
