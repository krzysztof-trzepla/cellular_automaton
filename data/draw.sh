#!/usr/bin/env bash

for file in `seq 0 $1`
do
    clear
    cat ./${file}.txt
    sleep 0.1
done