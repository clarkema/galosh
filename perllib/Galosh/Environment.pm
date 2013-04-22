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

package Galosh::Environment;

use Config

my %environment = ();
my %default     = ();

if ( $Config{'archname'} =~ m/^arm-linux/ ) {
    $default{'lisp_type'} = 'ccl';
    $default{'lisp_command'} = 'armcl';
}
else {
    $default{'lisp_type'} = 'sbcl';
    $default{'lisp_command'} = 'sbcl';
}

$environment{'lisp_type'}    = $ENV{'GALOSH_LISP_TYPE'}    ||
                               $default{'lisp_type'};
$environment{'lisp_command'} = $ENV{'GALOSH_LISP_COMMAND'} ||
                               $ENV{'GALOSH_LISP_TYPE'}    ||
                               $default{'lisp_command'};

sub lisp_type    { $environment{'lisp_type'} };
sub lisp_command { $environment{'lisp_command'} };

1;
