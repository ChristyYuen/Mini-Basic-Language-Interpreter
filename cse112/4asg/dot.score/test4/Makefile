# $Id: Makefile,v 1.6 2021-05-06 16:30:55-07 - - $

MKFILE   = Makefile
SOURCES  = main.c hello.c
HEADERS  = hello.h
OBJECTS  = main.o hello.o
CHECKIN  = ${MKFILE} ${SOURCES} ${HEADERS}
EXECBIN  = hello

all : ${EXECBIN}

${EXECBIN} : ${OBJECTS}
	gcc -g ${OBJECTS} -o ${EXECBIN}

%.o : %.c
	gcc -g -c $<

test : ${EXECBIN}
	./${EXECBIN} ; echo status = $$?

clean : 
	- rm ${OBJECTS} ${EXECBIN}

hello.o : hello.c hello.h
main.o : main.c hello.h

