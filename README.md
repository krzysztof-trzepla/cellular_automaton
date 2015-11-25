# cellular_automaton

Generic cellular automaton developed during Artificial Intelligence course at
AGH university of Science and Technology, Cracow 2015.

## Board structure

Cellular automaton board consists of tiles. Each tile neighbor with four
other tiles and is associated with one cellular worker. Tiles on the edge 
of the board are connected with each other, so that we achieve imitation 
of a infinite board. Moreover, board is divided into vertical sections, where
each section is placed on different node.

![Board structure](misc/images/board.png "Board structure.")

## Supervision tree

Cellular automaton consist of following components:
* cellular_automaton_sup
* cellular_worker_sup
* cellular_manager
* cellular_worker

Dependencies between those components are presented below.

![Supervision tree](misc/images/supervision_tree.png "Supervision tree.")

## Algorithm

Each cellular worker runs according to following algorithm:

![Algorithm](misc/images/algorithm.png "Algorithm.")

## Merging states

When it comes to merging states between cellular workers, each worker is responsible
for resolving conflicts between him an his bottom and right neighbour.

![Merging states](misc/images/merge_state.png "Merging state.")