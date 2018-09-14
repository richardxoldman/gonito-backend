#!/bin/bash -xe

rm -rf ../geval
git clone git://gonito.net/geval
mv geval ..
stack install
cp ~/.local/bin/gonito-bin .
tar zvcf gonito-distribution.tar.gz gonito-bin config/ static/
