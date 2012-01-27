#!/bin/sh

set -x

du -sh .

echo "Cleanning build files..."
make clean
cabal clean
ant clean

echo "Cleanning VC"
git gc
git gc --aggressive --prune=now

du -sh .

