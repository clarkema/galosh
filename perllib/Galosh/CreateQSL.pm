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

package Galosh::CreateQSL;

use Fcntl qw( :DEFAULT :flock );
use File::chdir;
use File::Spec;
use File::Temp;
use POSIX qw( strftime );
use JSON;

sub main
{
    local $/ = undef; # Slurp mode on

    my $temp_dir = File::Temp->newdir(
        DIR => File::Spec->join( $main::galosh_dir, 'tmp' ) );
    local $CWD = $temp_dir;

    open( my $template, '<', '/home/clarkema/git/galosh/qsl/qsl-single.tex' ) or die $!;

    my $tmp = File::Temp->new(
        DIR    => $temp_dir,
        SUFFIX => '.tex' );

    print $tmp latex_variables( from_json( <STDIN> ) );
    print $tmp $_ while <$template>;

    my $texname = $tmp->filename;
    (my $pdfname = $texname) =~ s/tex$/pdf/;

    $tmp->flush;
    $tmp->sync;

    system( "pdflatex $texname" );
    system( "evince $pdfname" );

    close( $template );
}

sub latex_variables
{
    my $info = shift;
    my $meta = shift @$info;
    my $swl = $meta->{'swl'};

    my $header = <<END;
\\documentclass[a4paper]{article}
\\pagestyle{empty}
\\usepackage{amsmath}
\\usepackage{latexsym}
\\usepackage{graphicx}
\\usepackage[absolute]{textpos}
\\usepackage[papersize={14cm,9cm}, margin=0.5cm, marginratio=1:1]{geometry}
\\renewcommand{\\familydefault}{\\sfdefault}
\\renewcommand{\\quad}{\\hspace*{2.5ex}}
\\setlength{\\parindent}{0pt}
\\setlength{\\TPHorizModule}{1cm}
\\setlength{\\TPVertModule}{1cm}
\\begin{document}
\\newcommand{\\contactline}[5]{\\vphantom{\$\\dfrac b b\$} #1 & #2 & #3 & #4 & #5 & PSE/TNX \\\\}
\\newcommand{\\emptycontact}{\\vphantom{\$\\dfrac b b\$} & & & & & \\\\}
\\newcommand{\\contactlines}{
    \\hline
END

    foreach my $qso (@$info) {
        if ( $swl ) {
            $header .= "\\contactline{$qso->{'his-call'}}{$qso->{'qso-date'}}{$qso->{'time-on'}}{$qso->{'qrg'}}{$qso->{'mode'}}";
        }
        else {
            $header .= "\\contactline{$qso->{'qso-date'}}{$qso->{'time-on'}}{$qso->{'qrg'}}{$qso->{'mode'}}{$qso->{'tx-rst'}}";
        }

        $header .= "\\hline";
    }

    $header .= <<END;
    }
\\newcommand{\\QsoRadio}{@{[ $meta->{'swl'} ? 'TO SWL' : 'QSO WITH' ]}}
\\newcommand{\\QsoHisCall}{$meta->{'to-call'}}
\\newcommand{\\QsoMyCall}{$meta->{'my-call'}}
\\newcommand{\\QsoMyITU}{$meta->{'my-itu-zone'}}
\\newcommand{\\QsoMyCQ}{$meta->{'my-cq-zone'}}
\\newcommand{\\QsoMyGrid}{$meta->{'my-grid'}}
\\newcommand{\\QsoMyAddress}{$meta->{'my-address'}}
\\newcommand{\\confirmation}{@{[ $meta->{'swl'}
    ? 'I am pleased to confirm your SWL report'
    : 'I am pleased to confirm our QSO'
    . ( scalar( @$info ) == 1 ? '' : 's' ) ]}:}

END

    if ( $swl ) {
        $header .= "\\newcommand{\\contacthead}{\\vphantom{\\rule{0pt}{12pt}}\\bf\\quad Worked \\quad & \\bf\\quad Date \\quad & \\bf\\ Time (UTC)\\ &\\bf\\quad MHz\\quad&\\bf\\ Mode\\ &\\bf QSL \\\\}\n";
    }
    else {
        $header .= "\\newcommand{\\contacthead}{\\vphantom{\\rule{0pt}{12pt}}\\bf\\quad Date \\quad & \\bf\\ Time (UTC)\\ &\\bf\\quad MHz\\quad&\\bf\\ Mode\\ &\\bf\\ RST \\ & \\bf QSL \\\\}\n";
    }

    return $header;
}

1;
