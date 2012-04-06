# galosh -- amateur radio utilities.
# Copyright (C) 2011, 2012 Michael Clarke, M0PRL
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

use 5.010;

use English qw( -no_match_vars );
use Fcntl qw( :DEFAULT :flock );
use File::chdir;
use File::Spec;
use File::Temp;
use POSIX qw( strftime );
use JSON;

use Getopt::Long qw( GetOptionsFromArray :config permute );

my $template = "$FindBin::RealBin/resources/qsl/default-qsl.tex";

sub main
{
    my $preview = 0;
    my $options_successful = GetOptionsFromArray( \@_,
        'preview' => \$preview,
    );
    exit 1 unless $options_successful;

    local $/ = undef; # Slurp mode on
    my $temp_dir;

    my $context_temp = File::Spec->join( $main::galosh_dir, 'tmp' );
    if ( $main::galosh_dir and -e $context_temp ) {
       $temp_dir = File::Temp->newdir( DIR => $context_temp );
    }
    else {
        $temp_dir = File::Temp->newdir( DIR => File::Spec->tmpdir() );
    }

    local $CWD = $temp_dir;

    open( my $template, '<', $template )
        or die $!;

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

    if ( $preview ) {
        preview_card( $pdfname );
    }
    else {
        print_card( $pdfname );
		#system( "lp -o media=Custom.90x140mm -o landscape $pdfname" );
    }

    close( $template );
}

sub preview_card
{
    my $file    = shift;
    my $command = $main::config->{'qsl.previewcmd'};

    if ( $command ) {
        system( "$command $file" );
    }
    else {
        given ( $OSNAME ) {
            when ( 'darwin' ) {
                system( "open -W $file" );
            }
            default {
                system( "evince $file" );
            }
        }
    }
}

sub print_card
{
    my $file    = shift;
    my $command = $main::config->{'qsl.printcmd'};

	if ( $command ) {
		system( "$command $file" );
	}
	else {
		system( "lp -o media=Custom.90x140mm -o landscape $file" );
	}
}

sub latex_variables
{
    my $info = shift;
    my $meta = shift @$info;
    my $swl = $meta->{'swl'};

    my $header = <<END;
\\documentclass{article}
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
\\newcommand{\\swlcontactline}[5]{\\vphantom{\$\\dfrac b b\$} #1 & #2 & #3 & #4 & #5 \\\\}
\\newcommand{\\qsocontactline}[5]{\\vphantom{\$\\dfrac b b\$} #1 & #2 & #3 & #4 & #5 & PSE/TNX \\\\}
\\newcommand{\\emptycontact}{\\vphantom{\$\\dfrac b b\$} & & & & & \\\\}
\\newcommand{\\contactlines}{
    \\hline
END

    foreach my $qso (@$info) {
        if ( $swl ) {
            $header .= "\\swlcontactline{$qso->{'his-call'}}{$qso->{'qso-date'}}{$qso->{'time-on'}}{$qso->{'qrg'}}{$qso->{'mode'}}";
        }
        else {
            $header .= "\\qsocontactline{$qso->{'qso-date'}}{$qso->{'time-on'}}{$qso->{'qrg'}}{$qso->{'mode'}}{$qso->{'tx-rst'}}";
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
        $header .= "\\newcommand{\\contacthead}{\\vphantom{\\rule{0pt}{12pt}}\\bf\\quad Worked \\quad & \\bf\\quad Date \\quad & \\bf\\ Time (UTC)\\ &\\bf\\quad MHz\\quad&\\bf\\ Mode\\\\}\n";
    }
    else {
        $header .= "\\newcommand{\\contacthead}{\\vphantom{\\rule{0pt}{12pt}}\\bf\\quad Date \\quad & \\bf\\ Time (UTC)\\ &\\bf\\quad MHz\\quad&\\bf\\ Mode\\ &\\bf\\ RST \\ & \\bf QSL \\\\}\n";
    }

    return $header;
}

1;
