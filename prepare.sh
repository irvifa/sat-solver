#!/bin/bash

URL=http://minisat.se/downloads/minisat-2.2.0.tar.gz
DIR=minisat
curl $URL | tar xvz
mv $DIR src/
