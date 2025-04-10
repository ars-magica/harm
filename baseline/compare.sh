#!/bin/sh

find . -name "*.md" -print | cut -c3- | \
while 
   read line
do
   diff "$line" ../pages"/$line"
done
