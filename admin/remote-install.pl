#!/usr/bin/perl -w
#   File      : /home/pss060/sls/flechsig/phase/admin/remote-install.pl
#   Date      : <20 Jan 00 12:18:51 flechsig> 
#   Time-stamp: <31 Jan 00 14:45:57 flechsig> 
#   Author    : Flechsig Uwe OVGA/203a 4535, flechsig@psi.ch

use Net::FTP;
use Net::Telnet;

my @targetsystem=("bessy","psicl0","pc2038","pc2346");
my @whatlist    =("phase","phasegui","phaselib");
my $releasedir  ="/home/pss060/sls/flechsig/phase/release/";


my $string1= join " ", @targetsystem;
my $string2= join " ", @whatlist;
my ($target,$what,$item,$item1,$oldir,$basename,$vmsname,%whathash)= ();


die "\nusage: $0  [targetsystem] [what]\n
     targetsystems: $string1
     what:          $string2\n" unless @ARGV; 

$target = $ARGV[0] || "pc2038";
$what   = $ARGV[1] || "phase";
$olddir= `pwd`;
print "my current dir is $olddir\n";
chdir $releasedir;
my @filelist= `ls -t`;
#print @filelist;
item:foreach $item (@whatlist)
{
item1:foreach $item1 (@filelist)
    {
	$_= $item1;
	m/(\w+)-(.+)\.tgz/;
	next item1 unless $1;
	if ($1 eq $item)
	{
	    $whathash{$item}= $1."-".$2.".tgz";  # wegen \n am ende
	    next item; 
	} 
    }
}
print "\nThe current releases are:\n";
foreach $item (keys %whathash)
{
    print "   $whathash{$item}\n";
}
die "\nerror: >>$what<< is unknown!\n" unless $whathash{$what}; 
$vmsname= $whathash{$what};
$vmsname=~ s/(\d+)\.(\d+)/$1p$2/;
$vmsname=~ m/(.+)\./;
$basename= $1;

if ($target eq $targetsystem[0])            #"bessy"
{
    print "install $whathash{$what} (vms: $vmsname) on $target\n"; 
    $ftp= Net::FTP->new("golden.exp.bessy.de", 
			Timeout=>30, 
			Debug =>1) or 
			    die "no connection to golden: $@\n";
    $ftp->login("flechsig","oskar5") or die "login denied\n"; 
    $ftp->cwd("phas.src") or die "can't cd to phas\n";
    $ftp->binary() or die "can't switch to binary\n";
    $ftp->put($whathash{$what},$vmsname) or 
	die "error during put $vmsname\n";
    $ftp->quit();

    print "Data transfered\n";
# ftp ist zuende
    $tn= Net::Telnet->new(Host => "tango1.exp.bessy.de", 
			  Timeout=>30, 
			  Prompt => '/FLECHSIG>/') or 
			      die "no connection to golden: $@\n";
    $tn->login("flechsig","oskar5") or die "login denied\n"; 
    print "telnet connection established\n";
    $tn->cmd("do phas") or die "error: do phase\n";
    $tn->cmd("\@phaseinit") or die "error: \@phaseinit\n";
    $tn->cmd("do src") or die "error: do src\n";
    $item1= $basename.".tar;*";
    print "send: del $item1";
    $item.= $tn->cmd("del $item1");
    print "send: gunzip $vmsname\n";
    $tn->print("gunzip $vmsname");
    ($item,$item1)= $tn->waitfor('/::FLECHSIG>/');
    print "item: $item, item1: $item1\n";
    $item1= $basename.".tar";
    $tn->cmd("world $item1") or die "error: world\n";
    print "send: vmstar -xvf $item1\n";
    $tn->print("vmstar -xvf $item1");
    ($item,$item1)= $tn->waitfor('/::FLECHSIG>/');
    print "item: $item, item1: $item1\n";
    print "end tar\n";
    
    $basename=~ s/(\d+)p(\d+)/$1_$2/;
    $item1= $basename.".dir";
    $tn->cmd("world $item1") or die "error: world\n";
    print "send: do $basename\n";
    $tn->cmd("do $basename") or die "error: do $basename\n";
    $tn->cmd("copy makefile.bessy makefile.") or die "error: copy makefile\n";
    print "start make\n";
    $tn->print("make");
    ($item,$item1)= $tn->waitfor(Match => '/::FLECHSIG>/',
				 Timeout => 600);
    print "item: $item, item1: $item1\n";
    print "end make";
    $tn->cmd("world *.*") or die "error: world\n";
    print "\nend telnet session\n";
    $tn->close;
#    print $item;
} 
elsif ($target eq $targetsystem[1])  # "psicl0"
{
    print "install $whathash{$what} (vms: $vmsname) on $target\n"; 
    $ftp= Net::FTP->new("psicl0.psi.ch", 
			Timeout=>30, 
			Debug =>1) or 
			    die "no connection to golden: $@\n";
    $ftp->login("flechsig","hallo123") or die "login denied\n"; 
    $ftp->cwd("phas.src") or die "can't cd to phas\n";
    $ftp->binary() or die "can't switch to binary\n";
    $ftp->put($whathash{$what},$vmsname) or 
	die "error during put $vmsname\n";
    $ftp->quit();

    print "ftp end: Data transfered\n";
# ftp ist zuende
#    exit;
    $tn= Net::Telnet->new(Host => "psicl0.psi.ch", 
			  Timeout=>60, 
			  Prompt => '/FLECHSIG>/') or 
			      die "no connection to psicl0: $@\n";
    $tn->login("flechsig","hallo123") or die "login denied\n"; 
    print "telnet connection established\n";
    $tn->cmd("import tools") or die "error: import tools\n";
    $tn->cmd("do phas") or die "error: do phase\n";
    $tn->cmd("\@phaseinit") or die "error: \@phaseinit\n";
    $tn->cmd("do src") or die "error: do src\n";
    $item1= $basename.".tar;*";
    print "send: del $item1\n";
    $item.= $tn->cmd("del $item1");
    print "send: gunzip $vmsname\n";
    $tn->print("gunzip $vmsname");
    ($item,$item1)= $tn->waitfor('/::FLECHSIG>/');
    print "item: $item, item1: $item1\n";
    $item1= $basename.".tar";
    $tn->cmd("world $item1") or die "error: world\n";
    print "send: vmstar -xvf $item1\n";
    $tn->print("vmstar -xvf $item1");
    ($item,$item1)= $tn->waitfor('/::FLECHSIG>/');
    print "item: $item, item1: $item1\n";
    print "end tar\n";
    
    $basename=~ s/(\d+)p(\d+)/$1_$2/;
    $item1= $basename.".dir";
    $tn->cmd("world $item1") or die "error: world\n";
    print "send: do $basename\n";
    $tn->cmd("do $basename") or die "error: do $basename\n";
    $tn->cmd("copy makefile.psicl0 makefile.") or die "error: copy makefile\n";
    print "start make\n";
    $tn->print("make");
    ($item,$item1)= $tn->waitfor(Match => '/::FLECHSIG>/',
				 Timeout => 600);
    print "item: $item, item1: $item1\n";
    print "end make";
    $tn->cmd("world *.*") or die "error: world\n";
    print "\nend telnet session\n";
    $tn->close;
#    print $item;
}
elsif ($target eq $targetsystem[2]) #local pc2038
{
    print "copy phase,phase.uid,news to /scratchl/phase/\n";
    `cp -p /home/pss060/sls/flechsig/phase/bin/phase     /scratchl/phase/bin/`;
    `cp -p /home/pss060/sls/flechsig/phase/lib/phase.uid /scratchl/phase/lib/`;
    `cp -p /home/pss060/sls/flechsig/phase/lib/news.     /scratchl/phase/lib/`;
} elsif ($target eq $targetsystem[3]) #pc2346
{
  print "install $whathash{$what} on $target\n"; 
    $ftp= Net::FTP->new("pc2346.psi.ch", 
			Timeout=>30, 
			Debug =>1) or 
			    die "no connection to pc2346: $@\n";
    $ftp->login("flechsig","hallo123") or die "login denied\n"; 
    $ftp->cwd("/usr/local/phase") or die "can't cd to phase\n";
    $ftp->binary() or die "can't switch to binary\n";
    $ftp->put($whathash{$what}) or 
	die "error during put\n";
    $ftp->quit();

    print "ftp end: Data transfered\n";
# ftp ist zuende
#    print $item;  
}else
{
    die ">>$target<< unknown system!\n";
}


chdir $olddir;
exit;
# /home/pss060/sls/flechsig/phase/admin/remote-install.pl
