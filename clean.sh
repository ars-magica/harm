#!/bin/sh

mkdir -p pages
rm -rf pages/*
( cd pages && ln -s ../Data/{Library,images,myst.yml} . )
