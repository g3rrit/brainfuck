#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

ocamlc -o $DIR/bf $DIR/bf.ml
$DIR/bf
rm $DIR/bf
rm $DIR/bf.cmi
rm $DIR/bf.cmo
