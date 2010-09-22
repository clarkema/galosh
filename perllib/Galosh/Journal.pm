# galosh -- amateur radio utilities.
# Copyright (C) 2010 Michael Clarke, M0PRL
# <clarkema -at- clarkema.org>
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

package Galosh::Journal;

use DBI;
use Fcntl qw( :DEFAULT :flock );
use File::Spec;
use File::Temp;
use POSIX qw( strftime );

sub main
{
    my $current_date = strftime( "%Y%m%d", gmtime() );

    my $dbh = DBI->connect(
        "dbi:SQLite:dbname=log.db",
        "", "",
        { RaiseError => 1 }
    ) or die $DBI::errstr;

    my $sth     = $dbh->prepare( "SELECT * FROM journal_entries WHERE journal_date = ? LIMIT 1" );
    my $updateh = $dbh->prepare( "UPDATE journal_entries SET text = ? WHERE id = ?" );
    my $inserth = $dbh->prepare( "INSERT INTO journal_entries (journal_date, text) VALUES (?, ?)" );

    $sth->execute( $current_date );

    if ( my $row = $sth->fetchrow_hashref() ) {
        # We have an existing entry to edit;
        my $id    = $row->{'ID'};
        my $date  = $row->{'JOURNAL_DATE'};
        my $entry = external_editor( $row->{'TEXT'} );
        $updateh->execute( $entry, $id );
    }
    else {
        # Make a new entry
        my $entry = external_editor( "" );

        if ( $entry ) {
            $inserth->execute( $current_date, $entry );
        }
    }

}

sub external_editor
{
    local $/ = undef;

    my $string = shift;
    my $tmp = File::Temp->new(
        DIR => File::Spec->join( $main::galosh_dir, 'tmp' ) );

    print( $tmp $string );
    $tmp->flush;
    $tmp->sync;

    flock( $tmp, LOCK_UN );
    system( $ENV{'EDITOR'}, $tmp->filename );

    open( my $infh, '<', $tmp->filename ) or die "Failed to open."; 

    my $new_string = <$infh>;

    close( $infh );

    return $new_string;
}

1;
