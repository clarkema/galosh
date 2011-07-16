install-debian-deps:
	aptitude install curl sbcl sqlite3 libsqlite3-dev libfile-homedir-perl \
	libfile-chdir-perl libjson-perl libdbi-perl libwww-perl libncurses5-dev \
	texlive-latex-base texinfo asciidoc

install-quicklisp:
	curl -O http://beta.quicklisp.org/quicklisp.lisp
	sbcl --no-userinit --load quicklisp --eval "(quicklisp-quickstart:install :path \"$(CURDIR)/quicklisp/\")" \
							--eval "(ql:quickload 'galosh)" \
							--eval "(quit)"
	rm quicklisp.lisp

ensure-deps:
	sbcl --no-userinit --load quicklisp/setup.lisp --eval "(ql:quickload 'galosh)" --eval "(quit)"

clean:
	rm -rf quicklisp

install-debian-packages:
	aptitude install curl sbcl sqlite3

test:
	prove
