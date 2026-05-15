#!/bin/bash

mkdir -p pages
rm -rf pages/*
( cd pages && ln -s ../Data/Library ../Data/images ../Data/myst.yml . )
