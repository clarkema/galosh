#!/usr/bin/perl -w

use FindBin;

sub prove_lisp
{
    my $package_name = shift;

    system( "sbcl", ( '--noinform',
                      '--noprint',
                      '--no-userinit',
                      '--eval', qq{(sb-sys:enable-interrupt sb-unix:sigint (lambda (x y z) (declare (ignore x y z)) (sb-ext:quit)))},
                      '--load', qq{$FindBin::RealBin/../quicklisp/setup.lisp},
                      '--eval', qq{(setf quicklisp-client:*local-project-directories* (list "$FindBin::RealBin/../src/"))},
                      '--load', qq{$FindBin::RealBin/../src/boot.lisp},
                      '--eval', qq{(let ((galosh-bootstrap:*main-package-name* "${package_name}"))(galosh-bootstrap:prove))},
                      '--eval' , "(quit)",
                      '--end-toplevel-options' ));
}

prove_lisp( 'galosh-qso' );
