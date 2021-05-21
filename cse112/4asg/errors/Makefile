# $Id: Makefile,v 1.1 2021-05-06 16:39:57-07 - - $

TARGETS = err0 err1 errn abrt seg zero foo invalid
SUBMAKE = make --no-print-directory

all :
	for target in ${TARGETS}; do \
	   echo make $$target:; \
	   ${SUBMAKE} $$target; \
	done

err0 :
	error 0

err1 :
	error 1

errn :
	error -1

abrt :
	abort

seg :
	segfault

zero :
	zerodivide

foo :
	foo bar baz

utime : utime-zero.txt

utime-zero.txt : utime-maxint.txt
	echo Need to make $@ from $<

