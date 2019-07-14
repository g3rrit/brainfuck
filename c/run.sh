#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

gcc -o $DIR/bf $DIR/bf.c
$DIR/bf
rm $DIR/bf
