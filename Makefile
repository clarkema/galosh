install-quicklisp:
	curl -O http://beta.quicklisp.org/quicklisp.lisp
	sbcl --load quicklisp --eval "(quicklisp-quickstart:install :path \"$(CURDIR)/quicklisp/\")" \
							--eval "(ql:quickload 'galosh)" \
							--eval "(quit)"
	rm quicklisp.lisp

ensure-deps:
	sbcl --load quicklisp/setup.lisp --eval "(ql:quickload 'galosh)" --eval "(quit)"

clean:
	rm -rf quicklisp
