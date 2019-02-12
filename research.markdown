Research {.f1}
========

### Critical phenomena in lattice polymers

![figure](/images/models.svg){.fr}
Most of my recent research focuses on the model of lattice self-avoiding
trails, analogous to self-avoiding walks but featuring bond-avoidance
rather than site-avoidance.
This research has produced a number of unexpected results.
I studied the collapse of trails in three dimension, showing that it
exhibits non-standard scaling where there is currently no theoretical
prediction available. I also explored how the phase diagram changes when we
introduce a second parameter such as stiffness or nearest-neighbour
interactions.

Lattice trails are not only an important problem in the study of polymeric
systems. The geometry of intersecting paths lies at the base of our
theoretical understanding of many completely different phenomena,
for example it is connected to the role of disorder in the Anderson
metal-insulator transitions in two dimensions. The problem of
characterising the collapse transition of trails is considered an
open problem in the field, with contrasting predictions, both
theoretical and numerical.

### Exact calculation of graph polynomials

I developed an algorithm to compute the Tutte polynomial of a graph, an object
of central importance in both mathematics and physics. My algorithm extends the
traditional approach based on the transfer matrix with the computer-science
concept of tree-decomposition. The result is two-fold: a software package to
compute the Tutte polynomial of a given graph faster than any other, and a very
general framework that can be easily adapted to compute many other kinds of
graph polynomials.

This work is of lasting importance because it allows the many researchers
working on the chromatic polynomial to study its properties, to state
conjectures and to test hypotheses in a fraction of the time required before.

The software is available on [GitHub](https://github.com/andreabedini/tutte)
and has DOI [10.5281/zenodo.15941](http://dx.doi.org/10.5281/zenodo.15941).

Using my algorithm I investigated the distribution of the roots of the
chromatic polynomial, over the ensemble of planar graphs. This is an area of
very active research given the numerous conjectures on the possible loci of
such roots that get stated and often disproven. The results of this study have
been published in Journal of Physics A
(DOI [10.1088/1751-8113/43/38/385001](http://dx.doi.org/10.1088/1751-8113/43/38/385001)).


### Super-symmetric methods in combinatorial problems

![figure](/images/hyperedge.svg){.fr}
My PhD research was devoted to the study of combinatorial problems through
the use of a formalism based on the Grassmann calculus. This is called a
“fermionic” formalism in reference to the Pauli principle, which states that
two half-spin particles (fermions) of the same kind cannot occupy the same
physical state. This principle is reflected in the nilpotency of algebraic
variables. Using this formalism I obtained formulas for enumerating the
forest configurations (subgraphs without cycles) one can draw on a graph.
My method is able to deal with forests on a more general structure, a
hypergraph, extending the results previously only available for ordinary
graphs. I proved that the asymptotic behaviour of the number of forests on a
hypergraph depends on the ratio between the number of connected components in
the forest (trees, in this case) and the number of vertices in the hypergraph.
This fact gives rise to a phase transition in the canonical ensemble, where a
fugacity parameter controls the number of trees.  Furthermore I proved that
this phase transition is consequence of the spontaneous breaking of a hidden
supersymmetry of the model and is associated with the appearance of a “giant
component”.

