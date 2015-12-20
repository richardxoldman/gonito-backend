#!/bin/bash

DATESTAMP=`date +%Y-%m-%d`
PACKAGE=gonito-$DATESTAMP.tar.gz

echo $PACKAGE

cp ~/.local/bin/gonito .
tar zvcf $PACKAGE gonito config/ static/
rm gonito
