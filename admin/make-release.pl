#!/usr/bin/perl -w
#  File      : /home/pss060/sls/flechsig/phase/admin/make-release.pl
#  Date      : <15 Oct 99 09:26:31 flechsig> 
#  Time-stamp: <22 Oct 99 16:29:02 flechsig> 
#  Author    : Flechsig Uwe OVGA/203a 4535, flechsig@psi.ch

# do something with a filelist

die "usage: filelistextension [release-number]\n" unless @ARGV; 

my $listname= $basename= $ARGV[0];
my $release= $ARGV[1] || time;
#$basename =~ s/filelist\.//;
$listname= "./admin/filelist.".$basename;
my $fname= "";
my $phasepath  ="/home/pss060/sls/flechsig/phase";
my $releasename="./release/$basename-$release.tgz";
my $sourcepath = "./src/".$basename."/";
my $currentpath=  `pwd`;
chop $currentpath; 
($currentpath eq $phasepath) ||
    die "\nYou are not in the right directory! $currentpath \ncd $phasepath\n\n";

print "listname: $listname, release $release, sourcepath: $sourcepath\n";
my $i= 0;
my @filelist= ();

open(LIST, $listname) or die "can't open file: $listname! $!\n";

line:while (<LIST>)
{
    $fname= $_;
    next line if /^#/;
    $fname= $sourcepath.$fname;
    chop $fname;
    push @filelist, $fname;
}
# filelist generated

foreach $fname (@filelist)
{
    print "file: $fname\n";
}

#print @filelist;
# option h dereferences links
`tar -czvhf$releasename @filelist`;
`chmod 444 $releasename`;

print "$releasename generated\n\n";
#`cd $actualpath`;
# end/home/pss060/sls/flechsig/phase/admin/filelist.pl
