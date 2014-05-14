thompson_nfa
============

Final project for 6.945, implementing Thompson NFA's for pattern matching

Design Choices:

a network is a hash table, where entries are nodes, and the key for a node will be an index number.
a node is a tagged list, (node . edges)
an edge is a tagged list, (edge predicate next-node-key)

a predicate takes in one argument which is data, and returns whether the edge should be traversed.
a blank edge has #t has its predicate, and thus will return true for an argument containing no data (the only such predicate)
at the end of step loop, spawn new probes along all blank edges recursively.
when adding new combinator, first node is the previous end node.

for blank edges:
two flags are required, leave-probe and expand-on-step
leave-probe indicates if a probe should be left on the source node when a blank edge is followed
expand-on-step indicates if a blank edge should be followed during the step loop of match:maker
