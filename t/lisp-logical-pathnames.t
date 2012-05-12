#!/usr/bin/perl -w

use Cwd qw( abs_path );
use File::Spec;
use FindBin;
use IPC::Open2;
use Test::More tests => 1;

use lib abs_path( File::Spec->join( $FindBin::RealBin,
                                    File::Spec->updir(),
                                    "local/lib/perl5" ) );
use File::HomeDir;

my $pid = open2(my $child_out, my $child_in,
    "sbcl", ( '--noinform',
                    '--noprint',
                    '--no-userinit',
                    '--load', qq{$FindBin::RealBin/../quicklisp/setup.lisp},
                    '--eval', qq{(setf quicklisp-client:*local-project-directories* (list "$FindBin::RealBin/../src/"))},
                    '--eval', qq{(let ((*standard-output* (make-broadcast-stream))(*error-output* (make-broadcast-stream))) (ql:quickload 'galosh-lisp))},
                    '--eval', qq{(in-package :galosh-lisp)},
                    '--eval', qq{(dolist (i (logical-pathname-translations "GL")) (princ i)(terpri))},
                    '--eval', "(terminate)",
                    '--end-toplevel-options' )
);

my @translations;
while ( <$child_out> ) {
    chomp;
    push @translations, $_;
}

my $correct_root = abs_path( File::Spec->join( $FindBin::RealBin,
                                               File::Spec->updir() ) );
my $correct_home = File::HomeDir->my_home;
my @correct_translations = (
    qq{(RES;**;*.*.* $correct_root/resources/**/*.*)},
    qq{(USR;**;*.*.* $correct_home/.galosh/**/*.*)},
);

is_deeply( \@translations, \@correct_translations,
           "Logical pathnames correct.");
