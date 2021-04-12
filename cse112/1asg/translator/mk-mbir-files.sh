#!/bin/sh
# $Id: mk-mbir-files.sh,v 1.2 2021-01-20 12:51:02-08 - - $

for mb in programs-mb.d/*.mb
do
   mbir=$(echo $mb | sed -e 's|.*/\(.*\)\.mb$|\1.mbir|')
   echo mbtran $mb mbir-files.d/$mbir
   mb2mbir.d/mbtran $mb >mbir-files.d/$mbir
done
