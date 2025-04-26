#!/bin/sh

cd baseline
find . -name "*.md" -print | cut -c3- | \
while 
   read line
do
   echo $line
   diff "$line" ../pages"/$line"
done
