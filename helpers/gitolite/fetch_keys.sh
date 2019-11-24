#!/bin/bash

for X in /var/keyswap/*.pub
do
    if [[ "$X" != "/var/keyswap/filipg.pub" && "$X" != "/var/keyswap/admin.pub" ]]
    then
	echo "COPYING $X"
	cp "$X" /var/lib/gitolite/.gitolite/keydir/
    fi	

done

gitolite trigger SSH_AUTHKEYS
