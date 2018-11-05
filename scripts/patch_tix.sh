#!/usr/bin/env bash

HPC_DIR=$(stack path --dist-dir)/hpc
GRAPHQL_API_MIX_ABSOLUTE_PATH=$(find $HPC_DIR -maxdepth 1 -name 'graphql-api-*')
GRAPHQL_API_MIX_PATH=$(basename $GRAPHQL_API_MIX_ABSOLUTE_PATH)
[[ -d tmp ]] || mkdir tmp

sed s/GRAPHQL_API_MIX_PATH/$GRAPHQL_API_MIX_PATH/ ticks/graphql-api.txt > tmp/graphql-api.txt
stack exec hpc -- overlay --hpcdir=$HPC_DIR --srcdir=. ./tmp/graphql-api.txt > tmp/graphql-api.tix
stack exec hpc -- combine $(stack path --local-hpc-root)/combined/all/all.tix tmp/graphql-api.tix --union > tmp/all.tix
