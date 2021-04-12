#!/bin/sh -x
# $Id: testrun.sh,v 1.11 2021-04-01 16:53:53-07 - - $

export PATH=$PATH:/afs/cats.ucsc.edu/courses/cse112-wm/bin

checksource README *.scm >check.log 2>&1
partnercheck >partnercheck.log 2>&1

./mbir.scm 00-hello-world.mbir  >00-hello-world.log  2>&1
./mbir.scm 01-1to10.mbir        >01-1to10.log        2>&1
./mbir.scm 02-exprs.mbir        >02-exprs.log        2>&1
./mbir.scm 10-exprs.mbir        >10-exprs.log        2>&1
./mbir.scm 11-let.mbir          >11-let.log          2>&1
./mbir.scm 12-syntax.mbir       >12-syntax.log       2>&1
./mbir.scm 20-goto.mbir         >20-goto.log         2>&1
./mbir.scm 21-let-if.mbir       >21-let-if.log       2>&1
./mbir.scm 22-fibonacci.mbir    >22-fibonacci.log    2>&1
./mbir.scm 23-pi-e-fns.mbir     >23-pi-e-fns.log     2>&1

echo 1 0 0   1 1 0   2 2 2 | \
./mbir.scm 30-quadratic.mbir    >30-quadratic.log    2>&1

echo 469 | \
./mbir.scm 31-collatz.mbir      >31-collatz.log      2>&1

echo 1 42 69 107 |  \
./mbir.scm 32-factorial.mbir    >32-factorial.log    2>&1

echo 5 1 4 2 3 10 1024 0 | \
./mbir.scm 40-sort-array.mbir   >40-sort-array.log   2>&1

./mbir.scm 41-eratosthenes.mbir >41-eratosthenes.log 2>&1

