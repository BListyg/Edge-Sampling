# Edge-Sampling

A question I was wondering recently was: how many different combinations of edges can be made from a complete graph with *n* nodes?

I wrote up some code to figure it.

The formal answer is:

if *n* is the number of nodes, then...

* for directed graphs (with loops): 2^(n^2) - 1

* for directed graphs (without loops): 2^(n*(n-1)) - 1

* for undirected graphs: 2^ ((n*(n-1)) / 2) - 1
