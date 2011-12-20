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

package Galosh::ClublogUpload;

use LWP;
use Getopt::Long qw( GetOptionsFromArray :config permute );

sub main
{
    my $clear = 0;
    my $options_successful = GetOptionsFromArray( \@_,
        'clear' => \$clear,
    );
    exit 1 unless $options_successful;

    my $upload_url      = "http://www.clublog.org/putlogs.php";
    my $file            = $_[1];
    my $target_callsign = $_[2];

    unless ( $file and $target_callsign ) {
        print STDERR "Usage: galosh clublog-upload <logfile> <callsign>\n";
        exit 1;
    }
    unless ( $file =~ m/\.(adif?|lgs)$/ )  {
        print STDERR "Error: Club Log only accepts .adif, .adi and .lgs ",
            "file extensions.\n";
        exit 1;
    }

    # This slurping of the log into a variable isn't really required,
    # but it provides a place to hook in any pre-processing we need to
    # do before uploading.
    my $content;
    open( my $fh, '<', $file ) or die $!;
    {
        local $/ = undef;
        $content = <$fh>;
    }
    close( $fh );

    my $ua = LWP::UserAgent->new(
        agent     => "galosh clublog-upload",
        env_proxy => 1,
    );
    my $res = $ua->post( $upload_url,
        Content_Type => 'multipart/form-data',
        Content      => [
            'email'    => $main::config->{'clublog.email'},
            'password' => $main::config->{'clublog.password'},
            'callsign' => $target_callsign,
            'clear'    => $clear,
            'file'     => [ undef, $file, Content => $content ], 
            'api'      => $main::clublog_api_key,
        ],
    );

    if ( $res->is_success ) {
        print STDERR "Upload successful:\n",
            $res->content, "\n";
    }
    else {
        print STDERR "Upload failed (", $res->status_line , "): \n",
            $res->content, "\n";
    }
}        

1;
