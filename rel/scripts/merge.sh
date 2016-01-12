#!/usr/bin/env bash

./merge_v.py ../cellular_automaton/data/langton_ant_0_0.dat ../cellular_automaton/data/langton_ant_1_0.dat langton_ant_0.dat
./merge_v.py langton_ant_0.dat ../cellular_automaton/data/langton_ant_2_0.dat langton_ant_0.dat

./merge_v.py ../cellular_automaton/data/langton_ant_0_1.dat ../cellular_automaton/data/langton_ant_1_1.dat langton_ant_1.dat
./merge_v.py langton_ant_1.dat ../cellular_automaton/data/langton_ant_2_1.dat langton_ant_1.dat

./merge_v.py ../cellular_automaton/data/langton_ant_0_2.dat ../cellular_automaton/data/langton_ant_1_2.dat langton_ant_2.dat
./merge_v.py langton_ant_2.dat ../cellular_automaton/data/langton_ant_2_2.dat langton_ant_2.dat

./merge_h.py langton_ant_0.dat langton_ant_1.dat langton_ant.dat
./merge_h.py langton_ant.dat langton_ant_2.dat langton_ant.dat

