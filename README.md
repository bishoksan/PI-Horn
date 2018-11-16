# Preconditions inferrer for Horn clauses (PI-Horn)

PI-Horn is a transformation-guided tool for inferring sufficient  preconditions for safety or unsafety of a program based on (Constrained) Horn clauses. It uses techniques such as abstract interpretation, partial evaluation, constraint specialisation and counterexample-guided program transformation and decomposition. 

PI-Horn is an iterative method which maintains over-approximations of the sets of safe and unsafe states which are successively refined using abstract interpreation and program transformations. Furthermore, in each iteration of the method, only the set of states in the intersection of these approximations are considered, which are initialised to a universal set at the beginning. The method terminates when the intersection of safe and unsafe states is empty or the intersection does not reduce in the successive iteration. Then the sufficient conditions are derived from the sequences of safe and unsafe over-approximations.



## Language and interface 

PI-Horn is written in Ciao Prolog and is interfaced with Parma polyhedra
library and Yices2 SMT solver for manipulating constraints.  It uses
several reusable components such as Convex polyhedra analyser,
Query-answer tranformer etc. It also includes a Java library for
manipulating finite tree automata.

## Requirements

[Ciao](https://github.com/ciao-lang/ciao) 1.16 or newer (installed
from git repository with `./ciao-boot.sh local-install`)

## Build and installation

You can automatically fetch, build, and install RAHFT using:

```sh
ciao get github.com/bishoksan/pihorn
```

The following dependendencies (including third-party code) will be
installed automatically:

1. [Ciao bindings](https://github.com/ciao-lang/ciao_ppl) for
   [Parma Polyhedra Library](https://bugseng.com/products/ppl/)
   (`ciao get ciao_ppl`)
2. [Ciao bindings](https://github.com/jfmc/ciao_yices) for
   [Yices SMT solver](https://yices.csl.sri.com/)
   (`ciao get github.com/jfmc/ciao_yices`)
3. [CHCLibs](https://github.com/bishoksan/chclibs)
   (`ciao get github.com/bishoksan/chclibs`)

All code will be downloaded and built under the first directory
specified in the `CIAOPATH` environment variable or `~/.ciao` by
default.

**For developing** it is recommended to define your own
_workspace directory_ and clone this repository. E.g., `export
CIAOPATH=~/ciao` and update your `PATH` with `eval "$(ciao-env)"`.
The dependencies can be cloned manually or fetched automatically by
calling `ciao fetch` at the source directory.

## Usage

**Usage**: `pihorn` \<*input file containing a set of Horn clauses*\>

**Input**: a set of Horn clauses together with `special clauses` for distinguished set of predicates. They
are written using Prolog notation:

e.g. a clause is written as: `h(X):- C, b1(X1),...,bn(Xn).` where `C` is a comma separated linear arithmetic constraints (`X>=10, Y=X+1`)

The distinguished predicates are `init(X)` (encodes the set of initial states), 'false' (encodes the set of unsafe states) and `safe` (encodes the set of safe terminating states). The `special clauses` are clauses of the above form with these distinugished predicates on the head, e.g.,  `false :- C, b1(X1),...,bn(Xn).` For technical reasons, we introduce two clauses, viz., `spec:- false.` and `spec:- safe.`, defining specification predicates.

**Output**: Sufficient preconditions for safety and unsafety of programs in terms of initial state variables.


## Generate a standalone binary distribution

```sh
mkdir dist; cd dist
ciaoc_sdyn ../src/pihorn
cp ../src/determinise.jar
```

This creates a platform specific binary `pihorn` at `dist/`
directory, together with the collection of shared libraries for the
dependencies.

## Using script to run the benchmarks
In order to run all the benchmarks containing in the foloder <Benchmarks> and produce statistics, please run the command

`./run_bench  <Benchmarks>`

The results will be in `result.txt`
## References


`An iterative approach to precondition inference using constrained Horn clauses`, TPLP-18 by Bishoksan Kafle, John P. Gallagher, Graeme Gange, Peter Schachte, Harald Søndergaard and  Peter J. Stuckey.


## Contact

Send your queries to `bishoksan@gmail.com`.
