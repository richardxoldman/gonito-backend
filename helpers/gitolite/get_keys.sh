#!/bin/bash

# script dumps public_keys from gonito DB and writes them into /var/keyswap
# the keys are read into gitolite by another script (fetch_keys.sh) run for a gitolie user

cd /var/keyswap
/home/gonito/get_keys.pl
