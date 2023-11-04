# GHS

An escript that tests an implementation of the GHS distributed algorithm.

## Build

*requires*: erlang, rebar3

* `rebar3 escriptize`

## Run

* `_build/default/bin/ghs N [-verbose]`
  * `N` numbers of nodes in the generated graph
  * `-verbose` log messages to `logs/log.txt`
* `_build/default/bin/ghs.cmd 50 | python utils/validate.py`
  * *requires*: python
  * Validate the output of a ramdom graph with 50 nodes. Script returns `0` if the solution is correct.
