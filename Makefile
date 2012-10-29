all:
	./setup

clean:
	rm -rf quicklisp

test:
	prove
