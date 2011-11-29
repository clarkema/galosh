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

package Galosh::DiffTool;

use File::Spec;
use File::Temp;

sub main
{
    my ($command, $rev) = @_;
    $rev ||= 'HEAD';

    my $context_temp = File::Spec->join( $main::galosh_dir, 'tmp' );
    if ( $main::galosh_dir and -e $context_temp ) {
       $temp_dir = File::Temp->newdir( DIR => $context_temp );
    }
    else {
        $temp_dir = File::Temp->newdir( DIR => File::Spec->tmpdir() );
    }

    my $new_db   = File::Spec->join( $main::galosh_dir, "log.db" );
    my $old_db   = File::Spec->join( $temp_dir, "old.db" );
    my $new_dump = File::Spec->join( $temp_dir, "new" );
    my $old_dump = File::Spec->join( $temp_dir, "old" );

    # FIXME Convert this lot to use IPC::Run as appropriate, to avoid
    # passing $rev etc. through the shell.  Also add error-handling on
    # the git command to catch unknown or invalid revisions.
    system( qq{echo '.dump' | sqlite3 $new_db > $new_dump} );
    system( qq{git cat-file blob $rev:.galosh/log.db > $old_db} );
    system( qq{echo '.dump' | sqlite3 $old_db > $old_dump} );
    system( qq{vimdiff -R $new_dump $old_dump } );
}

1;
