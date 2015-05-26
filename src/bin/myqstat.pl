#!/usr/bin/perl -w
# File      : /afs/psi.ch/project/phase/GIT/phase/src/bin/myqstat.pl
# Date      : <26 May 15 12:24:12 flechsig> 
# Time-stamp: <26 May 15 12:26:13 flechsig> 
# Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

# $Source$ 
# $Date$
# $Revision$ 
# $Author$ 

# ******************************************************************************
#
#   Copyright (C) 2015 Helmholtz-Zentrum Berlin, Germany and 
#                      Paul Scherrer Institut Villigen, Switzerland
#   
#   Author Johannes Bahrdt, johannes.bahrdt@helmholtz-berlin.de
#          Uwe Flechsig,    uwe.flechsig@psi.ch
#
# ------------------------------------------------------------------------------
#
#   This file is part of PHASE.
#
#   PHASE is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, version 3 of the License, or
#   (at your option) any later version.
#
#   PHASE is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with PHASE (src/LICENSE).  If not, see <http://www.gnu.org/licenses/>. 
#
# ******************************************************************************

# a compressed multi user queue status for clusters 

my $cmd= "qstat -u '*'";
my ($i, $line, @lines, %idh, @arr, $jid, %lh, %sh, @sline, $i0, $j, $k, 
    $mk, $slots, %idh1, %sh1)= (0,'',);

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
