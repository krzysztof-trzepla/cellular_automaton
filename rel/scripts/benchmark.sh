#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

${DIR}/../cellular_automaton/bin/cellular_automaton start
${DIR}/../cellular_automaton/bin/cellular_automaton ping
while [ $? -ne 0 ]; do
    sleep 1
    ${DIR}/../cellular_automaton/bin/cellular_automaton ping
done
${DIR}/../cellular_automaton/erts-7.1/bin/escript ${DIR}/benchmark.escript \
    ${DIR}/../cellular_automaton/data/report.csv
${DIR}/../cellular_automaton/bin/cellular_automaton stop
