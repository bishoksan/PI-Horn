#!/bin/bash

#the timeout command works for OSX. The results will be stored in result.txt.
progs=$1 # folder containing benchmarks
progs=$1
START1=$(date +%s000)
for file in $progs/*
do
  gtimeout  5m 	pihorn -pe  "$file"
done
END1=$(date +%s000)
DIFF1=$(( $END1 - $START1 ))
echo  " totaltime = $DIFF1 miliseconds"   >>  "result.txt"
