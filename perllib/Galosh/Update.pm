# galosh -- amateur radio utilities.
# Copyright (C) 2010, 2011 Michael Clarke, M0PRL
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

package Galosh::Update;

use File::Copy;
use File::Spec;
use IO::Uncompress::Gunzip qw( gunzip $GunzipError );
use LWP::UserAgent;

sub main
{
    my $cty_xml_url    = "http://www.clublog.org/cty.php?api=$main::clublog_api_key";
    my $cty_xml_file   = File::Spec->join( $main::home_galosh_dir, "cty.xml" );
    my $cty_xml_backup = $cty_xml_file . '.old';

    my $ua = LWP::UserAgent->new(
        agent     => "galosh update",
        env_proxy => 1,
    );
    my $req = HTTP::Request->new( GET => $cty_xml_url );
    my $res = $ua->request( $req );

    if ( $res->is_success ) {
        my $compressed = $res->content;
        my $uncompressed;

        gunzip( \$compressed, \$uncompressed )
            or die "Failed to decompress cty.xml: $GunzipError\n";
        
        copy( $cty_xml_file, $cty_xml_backup );
        open( my $fh, '>', $cty_xml_file )
            or die "Failed to open $cty_xml_file for writing: $!\n";

        print $fh $uncompressed;

        close( $fh );
    }
    else {
        print STDERR "Failed to download update cty.xml file: ", $res->status_line, "\n";
    }

}

1;
