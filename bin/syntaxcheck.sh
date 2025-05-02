#!/bin/sh

for i in Data/*.json 
do  
   echo "Checking file:" $i
   python3 -mjson.tool "$i"  > /dev/null && echo "No error"
done

