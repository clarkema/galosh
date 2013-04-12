# galosh -- amateur radio utilities.
# Copyright (C) 2013 Michael Clarke, M0PRL
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

package Galosh::ClublogActivity;

use LWP;
use URI;
use URI::QueryParam;
use Getopt::Long qw( GetOptionsFromArray :config permute );

our $activity_url = "http://www.clublog.org/activity_json.php";

sub main
{
    my $options = read_options( @_ );

    my $ua = LWP::UserAgent->new(
        agent     => "galosh clublog-activity",
        env_proxy => 1,
    );

    my $request_uri = URI->new( $activity_url );
    $request_uri->query_param( api    => $main::clublog_api_key );
    $request_uri->query_param( source => $options->{'source'} );
    $request_uri->query_param( dest   => $options->{'dest'} );

    {
        my $month = check_month( $options->{'month'} );
        $request_uri->query_param( 'month', $month ) if $month;
    }

    $request_uri->query_param( 'lastyear', '1' ) if $options->{'lastyear'};

    {
        my $min_sfi = check_sfi( $options->{'min-sfi'} );
        $request_uri->query_param( 'sfi', $min_sfi ) if $min_sfi;
        my $max_sfi = check_sfi( $options->{'max-sfi'} );
        $request_uri->query_param( 'usfi', $max_sfi ) if $max_sfi;
    }

    my $res = $ua->get( $request_uri );

    if ( $res->is_success ) {
        print $res->content, "\n";
    }
    else {
        print STDERR "Failed (", $res->status_line , "): \n",
            $res->content, "\n";
    }
}

sub read_options
{
    my $options = {};
    my $options_successful = GetOptionsFromArray( \@_, $options,
        'month=s'   ,
        'lastyear'  ,
        'min-sfi=i' ,
        'max-sfi=i' ,
    );
    exit 1 unless $options_successful;

    $options->{'source'} = $_[1];
    $options->{'dest'}   = $_[2];
    unless ( $options->{'source'} and $options->{'dest'} ) {
        print STDERR "Usage: galosh activity <src-adif> <dst-adif>\n";
        exit 1;
    }

    return $options;
}

sub check_month
{
    my $month = shift;
    return unless $month;
    if ( $month > 0 and $month < 13 ) {
        return $month;
    }
    else {
        die "Invalid month";
    }
}

sub check_sfi
{
    my $sfi = shift;
    return unless $sfi;
    if ( $sfi > 0 and $sfi < 500 ) {
        return $sfi;
    }
    else {
        die "Invalid SFI: $sfi";
    }
}

1;
