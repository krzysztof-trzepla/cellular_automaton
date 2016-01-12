#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ESCRIPT=`find ${DIR} -name escript`

${DIR}/../bin/cellular_automaton start
${DIR}/../bin/cellular_automaton ping
while [ $? -ne 0 ]; do
    sleep 1
    ${DIR}/../bin/cellular_automaton ping
done
${ESCRIPT} ${DIR}/benchmark.escript ${DIR}/../data/report.csv
${DIR}/../bin/cellular_automaton stop