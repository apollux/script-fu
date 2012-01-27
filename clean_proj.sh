#!/bin/sh

set -x

du -sh .

echo "Cleanning build files..."
[ -f "Makefile" ] && make clean
[ -f "build.xml" ] && ant clean
[ -f "pom.xml" ] && mvn clean

echo "Cleanning VC"
[ -d ".git" ] && git gc
[ -d ".git" ] && git gc --aggressive --prune=now

du -sh .

