#!/usr/bin/perl -w

my $cmd= "qstat -u '*'";
my ($i, $line, @lines, %idh, @arr, $jid, %lh, %sh, @sline, $i0, $j, $k, $mk, $slots, %idh1, %sh1)= (0,'',);

@lines= `$cmd`;        # store output
$line= shift @lines;   # skip 2 header lines
print ">" x 140, "\n";
print "i, k, n: $line";
print "=" x 140, "\n";

$line= shift @lines; 

# first compression
foreach $line (@lines)
{
    @arr= split " ", $line;
    $slots = ($arr[4] eq "r") ? $arr[8] : $arr[7]; 
    $mk= join "=", ($arr[0], $arr[3], $arr[4], $slots);
    $idh{$mk}++;     # how much
    $lh{$mk}= $i;    # remember last line
    $i++;
}
$i0= $i;

# second compression
foreach $jid (keys %idh)
{
    @arr= split "=", $jid;
    $mk= join "=", ($arr[1], $arr[2], $arr[3]);
    $idh1{$mk}+= $idh{$jid} if $idh{$jid} > 1;
    $idh1{$mk}+= $arr[3] if $arr[3] > 1;
    $lh1{$mk}= $lh{$jid};
#    print "jid=$jid, n(id)=$idh{$jid}, line=$lines[$lh{$jid}]";
#    push @sline, $lh{$jid};
#    print "$mk\n";
    $j++
}

foreach $jid (keys %idh1)
{
    push @sline, $lh1{$jid};
    $j++
}

$k=0;
foreach $i (sort { $a <=> $b } @sline)
{
    @arr= split " ",  $lines[$i];
    $slots = ($arr[4] eq "r") ? $arr[8] : $arr[7];
    $jid= join "=", ($arr[3], $arr[4], $slots);
#    print "$k,\t$i,\t$idh1{$jid},\t$jid\t: $lines[$i]";
    printf "%3d %4d %4d %-20s : %s", $k, $i, $idh1{$jid}, $jid, $lines[$i];
#    print "k=$k: $lines[$i]";
    $k++;
}
print "=" x 140, "\n";
print "summary: i0= $i0, j= $j, user(s)= $k\n";
print "<" x 140, "\n";
#print @sline;
#end
