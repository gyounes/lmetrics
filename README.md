lmetrics
=====

An application used to record metrics.
Metrics that can be recorded for now are: transmission, memory, latency.

Transmission: messages, acks, resent messages..
Memory: CRDTs and other data structures needed in the process (vector clocks, ...)
Latency: locally and remotely

Build
-----

    $ rebar3 compile
