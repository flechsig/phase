#! /usr/bin/perl -w
#   File      : /home/pss060/sls/flechsig/phase/utils/create-coefficients.pl
#   Date      : <02 Apr 01 10:58:06 flechsig> 
#   Time-stamp: <06 Apr 01 08:34:24 flechsig> 
#   Author    : Flechsig Uwe OVGA/203a 4535, flechsig@psi.ch

# define variables
my ($ch, $shape, $fname, $i, $j, $aij, @coeff, $r, $rho, $l, $a, $tmp) = ();
my ($input, $today)= ();

$today= localtime;

print "=" x 80, "\n";
print "create coefficient file for PHASE\n";
print "=" x 80, "\n";

print "input filename: ";
$fname= <STDIN>;
chomp $fname;
if (-e $fname)
{
   print "\nfile \"$fname\" exists: do you really want to overwrite? (y/n) ";
   $ch= <STDIN>;
   chomp $ch;
   die "we do not overwrite \"$fname\"- exit\n" unless $ch eq 'y';
}
open (FILE, "> $fname") or die "cant open file \"$fname\", $!\n";
print "input shape: (c)one, (t)oroid: ";
$ch= <STDIN>;
chomp $ch;
######################## toroid #####################################
if ($ch eq "t")
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
       $tmp= 1.0/ (8.0* $rho*3);
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
} # end toroid
######################## cone #####################################
elsif ($ch eq "c")
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
else
{
   die "$ch - unknown shape, exit\n"; 
}
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

while (($i,$j,$aij)= splice (@coeff, 0, 3))
{
    print FILE "$i $j $aij\n"
}
print FILE "# end $fname\n";
close(FILE);
print "file \"$fname\" generated\n";
# end /home/pss060/sls/flechsig/phase/utils/create-coefficients.pl
