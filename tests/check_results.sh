#!/bin/bash

set -eu

function find_bench_file {
    BENCH_FILE=$(ls -t *.bench | head)

    if [ -z "$BENCH_FILE" ]
    then
        echo "No benchmark file found. Exiting"
        exit 1
    fi

    echo -n "$BENCH_FILE"
}

BENCH1=$1
BENCH2=$2
BENCH2_EXPECTED_OUT=$3

BENCH1_OUT=$(racket $BENCH1)
BENCH1_FILE=$(find_bench_file)

BENCH2_OUT=$(racket $BENCH2)
BENCH2_FILE=$(find_bench_file)

if [ ! "$BENCH1_FILE" == "$BENCH2_FILE" ]
then
    echo "$BENCH1 and $BENCH2 use different .bench files"
    echo "$BENCH1 : $BENCH1_FILE"
    echo "$BENCH2 : $BENCH2_FILE"
    echo "Cannot make a comparison. Exiting"
    exit 1
fi

if [ $( echo "$BENCH2_OUT" | grep -c "$BENCH2_EXPECTED_OUT") == "0" ]
then
    echo "FAILURE: Expected output $BENCH2_EXPECTED_OUT not found"
    echo "BENCH1_OUT: $BENCH1_OUT"
    echo "BENCH2_OUT: $BENCH2_OUT"
    exit 1
else
    echo "SUCCESS: Expected output $BENCH2_EXPECTED_OUT found"
    exit 0
fi

