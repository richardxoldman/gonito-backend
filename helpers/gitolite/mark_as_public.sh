#!/bin/bash

REPODIR="$1"

if [[ ! -d "$REPODIR" ]]
then
    echo "NO DIRECTORY!"
    exit 1
fi

chgrp -R git "$REPODIR"
chmod -R g+rX "$REPODIR"

find "$REPODIR" -type d -exec chmod g+s '{}' ';'


