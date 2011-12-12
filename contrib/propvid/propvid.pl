#!/usr/bin/perl -w

# galosh -- amateur radio utilities.
# Copyright (C) 2011 Michael Clarke, M0PRL
# <mike -at- galosh.org.uk>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License Version 3, as
# published by the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

use 5.010;

# Version the file.
# Best practices use the CPAN version module with three parts to the version
# number.  KEEP THIS ALL ON ONE LINE OR MODULE LOADERS WILL FAIL.
use version; our $VERSION = qv('0.0.1');

# Use strict and detect problems like we mean it.
# There is no such thing as "just a warning" -- if the code is not clean then
# the code is not right.  All warnings are on, all warnings are errors.
use strict;
use warnings 'all';
use warnings FATAL => 'all';

# Core/CPAN modules to import
use File::Spec;
use File::Temp;
use Getopt::Long;
use Pod::Usage;

# Help option variables
my $help;
my $usage;
my $version;

my $base_call;
my $base_qth;
my $base_date = '';

# Program option variables
my $options_successful = GetOptions(
    # Help options - these take priority with standard 
    'h|help'           => \$help,
    'u|usage'          => \$usage,
    'v|version'        => \$version,
    # Program options
    'c|call:s'         => \$base_call,
    'q|qth:s'          => \$base_qth,
    'd|date:s'         => \$base_date,
);

if ( not $options_successful or $help ) {
    pod2usage( { -exitval => 1, -verbose => 1, -output => \*STDERR } );
}

if ( $usage ) {
    pod2usage( { -exitval => 1, -verbose => 0, -output => \*STDERR } );
}

# Check number of non-option arguments here
#if ( @ARGV == 0 or @ARGV > 1 ) {
#    pod2usage( { -exitval => 1, -verbose => 0, -output => \*STDERR } );
#}
#
if ( $version ) {
    print {*STDERR} "propvid version $VERSION by Michael Clarke\n";
    exit 1;
}

unless ( $base_call and $base_qth ) {
    pod2usage( { -exitval => 1, -verbose => 0, -output => \*STDERR } );
}
unless ( $base_date =~ m/\d{8}/ ) {
    print STDERR "--date should be of the form '20110930'\n";
    exit 1;
}
    
our %colour_for_band = (
    '160m' => 'red',
    '80m'  => 'purple',
    '20m'  => 'green',
    '17m'  => 'blue',
    '15m'  => 'orange',
    '12m'  => 'white',
    '10m'  => 'yellow',
);

my $file         = shift;
my $nodes        = {};
my $base_node    = '';
my $tmp_dir_base = '/tmp';

unless ( -e $tmp_dir_base ) {
    die "Temporary directory $tmp_dir_base does not exist.";
}

my $tmp_dir = File::Temp->newdir ( DIR => $tmp_dir_base );

{
    my ($lat, $long) = locator2latlong( $base_qth );
    $base_node = qq{$lat\t$long\t"$base_call"\tcolor=white align=left\n};
}

open( my $fh, '<', $file ) or die $!;
    while ( my $line = <$fh> ) {
        my @line     = split( ',', $line );
        my $time     = convert_unix_time( $line[1] );
        my $operator = $line[2];
        my $mygrid   = $line[3];
        my $call     = $line[6];
        my $hisgrid  = $line[7];
        my $freq     = $line[5];

        if ($freq) {
            if ( $mygrid and $operator ne $base_call ) {
                $nodes->{$time}{$operator}{'grid'} = $mygrid;
                $nodes->{$time}{$operator}{'band'} = qrg2band( $freq );
            }
            if ( $hisgrid and $call ne $base_call ) {
                $nodes->{$time}{$call}{'grid'} = $hisgrid;
                $nodes->{$time}{$call}{'band'} = qrg2band( $freq );
            }
        }
        else {
            print STDERR "No frequency data for line: $line\n";
        }
    }
close( $fh );

#
# This section is to support ADIF files instead of WSPRnet logs;
# primarily for use with PSKreporter files.  Needs to be
# properly integrated and given its own option.
#
#open( my $fh, '<', $file ) or die $!;
#    while ( my $line = <$fh> ) {
#        my ($time)     = $line =~ m/<TIME_ON:\d+>(\d{3})/i;
#        $time .= "000";
#        my ($operator) = $line =~ m/<OPERATOR:\d+>([^ <]*)/i;
#        my ($mygrid)   = $line =~ m/<MY_GRIDSQUARE:\d+>([^ <]*)/i;
#        my ($call)     = $line =~ m/<CALL:\d+>([^ <]*)/i;
#        my ($hisgrid)  = $line =~ m/<GRIDSQUARE:\d+>([^ <]*)/i;
#        my ($freq)     = $line =~ m/<FREQ:\d+>([^ <]*)/i;
#
#        if ($freq) {
#            if ( $mygrid and $operator ne $base_call ) {
#                $nodes->{$time}{$operator}{'grid'} = $mygrid;
#                $nodes->{$time}{$operator}{'band'} = qrg2band( $freq );
#            }
#            if ( $hisgrid and $call ne $base_call ) {
#                $nodes->{$time}{$call}{'grid'} = $hisgrid;
#                $nodes->{$time}{$call}{'band'} = qrg2band( $freq );
#            }
#        }
#        else {
#            print STDERR "No frequency data for line: $line\n";
#        }
#    }
#close( $fh );

my $i = 0;
for my $hour (00..23) {
    for my $ten (0..5) {
        my $time = sprintf("%02d%s000", $hour, $ten);
        my $datetime = "$base_date.$time";
        open( my $fh, '>', File::Spec->join( $tmp_dir, "markers" ) ) or die $!;
        print $fh $base_node;
        print $fh qq{-10 900 "}, sprintf("%02d%s0", $hour, $ten),
            qq{z" image=none position=pixel color=black outlined=false\n};
        while ( my ($k, $v) = each( %{$nodes->{$time}} ) ) {
            my ($lat, $long) = locator2latlong( $v->{'grid'} );
            print $fh qq{$lat\t$long\t"}, $v->{'band'},
                qq{"\tcolor=}, band2colour( $v->{'band'} ), qq{\n};
        }
        close( $fh );

        open( $fh, '>', File::Spec->join( $tmp_dir, "config" ) ) or die $!;
        print $fh "[earth]\n";
        print $fh "marker_file=" . File::Spec->join( $tmp_dir, "markers" ) . "\n";
        close( $fh );

        my $jpg = sprintf("%03d", $i++);
        system( "xplanet -projection mercator -geometry 1000x500 -config " .
            File::Spec->join( $tmp_dir, "config" ) . " -output " .
            File::Spec->join( $tmp_dir, "$jpg.jpg" ) .
            " -num_times 1 -date $datetime -quality 100" );
    }
}

system( "ffmpeg -r 2 -y -i " .
    File::Spec->join( $tmp_dir, '%03d.jpg' ) . " -vcodec libx264 output.mp4" );

sub locator2latlong
{
    my $locator = uc(shift);
    my $A = ord('A');
    my $long = (ord( substr( $locator, 0, 1 ) ) - $A) * 20;
    my $lat  = (ord( substr( $locator, 1, 1 ) ) - $A) * 10;

    if ( length( $locator ) > 2 ) {
        $long += substr( $locator, 2, 1 ) * 2;
        $lat  += substr( $locator, 3, 1 );
    }

    if ( length( $locator ) > 4 ) {
        $long += (ord( substr( $locator, 4, 1 ) ) - $A) * 5/60;
        $lat  += (ord( substr( $locator, 5, 1 ) ) - $A) * 5/120;
    }

    return ($lat - 90, $long - 180);
}

sub qrg2band
{
    my $qrg = shift;
    $qrg *= 1_000_000;

    given ( $qrg ) {
        when ( $_ >= 1800000   && $_ <= 2000000)   { return "160m" }
        when ( $_ >= 3500000   && $_ <= 4000000)   { return "80m" }
        when ( $_ >= 5330500   && $_ <= 5403500)   { return "60m" }
        when ( $_ >= 7000000   && $_ <= 7400000)   { return "40m" }
        when ( $_ >= 10100000  && $_ <= 10150000)  { return "30m" }
        when ( $_ >= 14000000  && $_ <= 14350000)  { return "20m" }
        when ( $_ >= 18068000  && $_ <= 18168000)  { return "17m" }
        when ( $_ >= 21000000  && $_ <= 21450000)  { return "15m" }
        when ( $_ >= 24890000  && $_ <= 24990000)  { return "12m" }
        when ( $_ >= 28000000  && $_ <= 29700000)  { return "10m" }
        when ( $_ >= 50000000  && $_ <= 54000000)  { return "6m" }
        when ( $_ >= 144000000 && $_ <= 148000000) { return "2m" }
        default {
            print STDERR "Unrecognized band for frequency: $qrg\n";
            return "";
        };
    }
}

sub convert_unix_time
{
    my @unix  = gmtime( shift );
    my $hours = length( $unix[2] ) == 1 ? "0$unix[2]" : $unix[2];
    my $mins  = int( $unix[1] / 10 );
    return "$hours${mins}000";
}

sub band2colour
{
    my $band = shift;
    return $colour_for_band{ $band } || 'red';
}

__END__

=head1 NAME

propvid - Produce videos of radio propagation over time.

=cut

=head1 SYNOPSIS

B<propvid> -c YOURCALL -q YOURLOCATOR -d DATE <logfile>

=cut

=head1 DESCRIPTION

LONG DESCRIPTION

=cut

=head1 OPTIONS

=over

=item B<-h>, B<--help>

Print the help and exit.

=item B<-u>, B<--usage>

Print a brief usage summary and exit.

=item B<-v>, B<--version>

Print the version string and exit.

=item B<-c>, B<--call>

Base Call

=item B<-q>, B<--qth>

Base QTH.

=back

=item B<-d>, B<--date>

The date to use when calculating the position of the terminator.

=back

=cut

=head1 BUGS

Please report bugs to <mike -at- galosh.org.uk>.

=cut

=head1 AUTHOR

Written by Michael Clarke.

=cut

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2011, Michael Clarke <mike -at- galosh.org.uk>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License Version 3, as
published by the Free Software Foundation.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
