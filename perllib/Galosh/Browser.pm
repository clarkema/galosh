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

package Galosh::Browser;

use File::Glob ':glob';
use File::Spec;
#use IO::Socket::UNIX qw( SOCK_STREAM );

sub send_command
{
    my $fifo_path = shift;
    my $command   = shift;

    open ( my $fh, '>', $fifo_path )
        or die "Error: Failed to open FIFO $fifo_path: $!\n";

    print $fh $command . "\n";

    close( $fh );
}

#sub send_command
#{
#    my $socket_path  = shift;
#    my $command = shift;
#
#    if ( not ( -S $socket_path ) ) {
#        die "Error: $socket_path is not a socket.\n";
#    }
#    else {
#        my $sock = IO::Socket::UNIX->new(
#            Type => SOCK_STREAM,
#            Peer => $socket_path,
#        ) or die "Failed to open socket $socket_path: $!\n";
#
#        print $sock $command;
#        $sock->close;
#    }
#
#}

sub find_first_tab_fifo
{
    my $search_dir = shift;
    my @fifos      = bsd_glob( "$search_dir/uzbl_fifo_*-1" );
    my $num_fifos  = scalar @fifos;

    if ( $num_fifos == 0 ) {
        return;
    }
    elsif ( $num_fifos == 1 ) {
        return $fifos[0];
    }
    else {
        die "Multiple FIFOs in $search_dir.  Either some are stale, or there\n" .
            "are multiple instances of the browser running.  Please clean up\n" .
            "and try again.\n";
    }
}

sub spawn_browser
{
    my $tmp_dir = shift;
    my $url     = shift || "about:blank";

    if ( $pid = fork ) {
        # Parent
        return;
    }
    else {
        # Child
        exec( "uzbl-tabbed", ( '--socketdir', $tmp_dir,
                               '--fifodir',   $tmp_dir, $url ) );
    }

}

sub main
{
    my ($command, $url) = @_;
    my $tmp_dir = File::Spec->join( $main::galosh_dir, 'tmp' );
    my $fifo    = find_first_tab_fifo( $tmp_dir );

    if ( $url =~ m!^[a-z0-9/]+$! ) {
        $url = "http://www.qrz.com/db/$url";
    }

    if ( $fifo ) {
        send_command( $fifo, "uri $url" );
    }
    else {
        spawn_browser( $tmp_dir, $url );
    }
}

1;
