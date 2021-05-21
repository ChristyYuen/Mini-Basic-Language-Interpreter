# $Id: Makefile,v 1.5 2021-03-02 15:32:35-08 - - $

MKFILE   = Makefile
SOURCES  = main.c hello.c
HEADERS  = hello.h
OBJECTS  = main.o hello.o
CHECKIN  = ${MKFILE} ${SOURCES} ${HEADERS}
EXECBIN  = hello

all : ${EXECBIN}

${EXECBIN} : ${OBJECTS}
	cc -g ${OBJECTS} -o ${EXECBIN}

%.o : %.c
	cc -g -c $<

test : ${EXECBIN}
	./${EXECBIN} ; echo status = $$?

clean : 
	- rm ${OBJECTS} ${EXECBIN}

hello.o : hello.c hello.h
main.o : main.c hello.h

