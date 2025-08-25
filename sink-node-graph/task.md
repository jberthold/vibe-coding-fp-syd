# A Special Graph Implementation

The application we would like to program here is a special implementation of a directed graph.
Its only two methods are `addEdge`, and `leavesFrom`.

## Operations

The nodes of the graph are identified by (arbitrary) string labels.
Edges of the graph are pairs of such labels, a pair `("a", "b")` denotes an edge from node `"a"` to node `"b"`.
The graph is constructed using the `addEdge` operation, and queried using the `leavesFrom` operation

### `addEdge`

The `addEdge` operation takes an existing graph and an edge (given as a pair of labels), and adds the edge to the graph.
If the start or the end of the edge are not already nodes in the graph, they are added automatically.
There are no restrictions to what edges are allowed.
The operation does not return anything.

## `leavesFrom`

The `leavesFrom` operation takes a label, and returns the set of all _leaves_, i.e., nodes without any out-going edges, reachable from the node with the given label.
If no node with this label exists, the resulting set is empty.

It is very important to make `leavesFrom` efficient.

## Program Interface

We would like to have a `main` program to test this implementation.
The program starts with an empty graph, having no nodes and no edges.
It first reads edges (pairs of labels), one edge per line, from a text file and insert each edge into the graph using `addEdge`.
Then it will read a second file with labels, one per line, and query `leavesFrom` for each of the given labels on the graph obtained before by adding the edges.

The output of the test program are all sets of leaves returned by `leavesFrom`, one set per line, corresponding to the labels from the second file.
