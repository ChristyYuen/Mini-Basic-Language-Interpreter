# $Id: Makefile,v 1.13 2020-09-06 20:13:06-07 - - $

#
# General useful macros
#

MKFILE     = Makefile
MAKEFLAGS += --no-builtin-rules
DEPSFILE   = ${MKFILE}.deps
NOINCLUDE  = ci clean spotless
NEEDINCL   = ${filter ${NOINCLUDE}, ${MAKECMDGOALS}}

#
# File list macros
#

EXECBIN	   = mbtran
OBJCMX	   = absyn.cmx etc.cmx parser.cmx lexer.cmx main.cmx
OBJCMI	   = ${patsubst %.cmx, %.cmi, ${OBJCMX}}
OBJBIN	   = ${patsubst %.cmx, %.o, ${OBJCMX}}
MLSOURCE   = etc.mli absyn.ml etc.ml main.ml
GENSOURCE  = parser.mli parser.ml lexer.ml 
GENFILES   = ${GENSOURCE} parser.output
ALLSOURCES = README ${MLSOURCE} parser.mly lexer.mll ${MKFILE}

#
# General targets
#

all : ${EXECBIN}

${EXECBIN} : ${OBJCMX}
	ocamlopt ${OBJCMX} -o ${EXECBIN}

%.cmi : %.mli
	ocamlc -c $<

%.cmx : %.ml
	ocamlopt -c $<

%.ml : %.mll
	ocamllex $<

%.mli %.ml : %.mly
	ocamlyacc -v $<

#
# Misc targets
#

clean :
	- rm ${OBJCMI} ${OBJCMX} ${OBJBIN}

spotless : clean
	- rm ${EXECBIN} ${GENFILES} Listing.ps Listing.pdf

ci : ${ALLSOURCES}
	cid -is ${ALLSOURCES}

deps : ${MLSOURCE} ${GENSOURCE}
	@ echo "# ${DEPSFILE} created `date`" >${DEPSFILE}
	ocamldep ${MLSOURCE} ${GENSOURCE} >>${DEPSFILE}

${DEPSFILE} :
	@touch ${DEPSFILE}
	${MAKE} deps

lis : ${ALLSOURCES}
	mkpspdf Listing.ps ${ALLSOURCES}

again :
	${MAKE} spotless
	${MAKE} deps
	${MAKE} ci
	${MAKE} all
	${MAKE} lis

ifeq "${NEEDINCL}" ""
include ${DEPSFILE}
endif

