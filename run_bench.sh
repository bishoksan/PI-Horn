#!/bin/bash

#the timeout command works for OSX. The results will be stored in result.txt.
progs=$1 # folder containing benchmarks
START=$(date +%s000)
for file in $progs/*.pl
do
  gtimeout  5m 	pihorn -pe  "$file"
done
END=$(date +%s000)
DIFF=$(( $END - $START ))
echo  " totaltime = $DIFF miliseconds"   >>  "result.txt"
