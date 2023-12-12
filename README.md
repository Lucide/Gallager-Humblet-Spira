# GHS

An escript that tests an implementation of the GHS distributed algorithm.

## Build

*requires*: erlang, rebar3

* `rebar3 escriptize`

## Run

*requires*: python

* `python ./pysrc/geometric_graph.py -n 50` or `python ./pysrc/planar_graph.py -n 20`
* `_build/default/bin/ghs | python pysrc/validate.py ./json/graph.json`
  * Validate the output with NetworkX. Script returns `0` if the solution is correct.
