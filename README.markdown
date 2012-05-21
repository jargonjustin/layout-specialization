## Attribute Grammar Layout Engine Specialization

An attribute grammar based specializing layout engine written in OCaml. Layout
algorithms may be specified with an attribute grammar, defining how various
visual layout properties are computed from arbitrary tree data types. The
attribute grammar will be specialized to a particular input tree prior
to interpretation and rendering.

This prototype was developed as a student research project for the University
of California, Berkeley's [Parallel Web Browser][pbrowser] project under the
direction of [Rastislav Bodík][ras]. See [Leo Meyerovich's][leo] work on the
[Fast Tree Language][ftl] for a more modern incarnation of similar ideas.

[pbrowser]: http://parlab.eecs.berkeley.edu/research/80 "Parallel Web Browser"
[ras]: http://www.cs.berkeley.edu/~bodik/ "Rastislav Bodík"
[leo]: http://www.eecs.berkeley.edu/~lmeyerov/ "Leo Meyerovich"
[ftl]: http://www.eecs.berkeley.edu/~lmeyerov/#ftl "Fast Tree Language"
