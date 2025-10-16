# LispNet Functions

## Creation

All creation algorithms make graphs with `n` nodes labeled `0..n-1`.

### make-digraph

**Signature:** `()`

Create a null graph.

### make-empty

**Signature:** `(n)`

Create a graph with `n` nodes and no edges.

### make-line

**Signature:** `(n &key (bi nil))`

Create a line graph with `n` nodes where node `i` points to node `i+1`. When `bi` is non-`nil`, node `i+1` additionally points to node `i`.

### make-ring

**Signature:** `(n &key (bi nil))`

Create a ring graph with `n` nodes; same as `make-line` but node `n-1` points to node `0`. When `bi` is non-`nil`, node `0` additionally points to node `n-1`.

### make-ring-lattice

**Signature:** `(n k &key (bi nil))`

Create a ring lattice with `n` nodes of degree `k`. When `k=0` this function is equivalent to `make-empty` and when `k=2` this function is equivalent to `make-ring`. When `bi` is `nil` each outgoing edge from a source node is pointed to the closest possible node around the ring which isn't already pointed to by that source node.

### make-star

**Signature:** `(n &key (direction :bi))`

Create a star graph `n` external nodes and a central node (total of `n+1` nodes).

| `direction` | Description                                                           |
|-------------|-----------------------------------------------------------------------|
| `:bi`       | Edges point both to and from the central node and each external node. |
| `:in`       | Edges only point from each external node to the central node.         | 
| `:out`      | Edges only point from the central node to each external node.         |

### make-complete

**Signature:** `(n &key (allow-self nil))`

Create a complete graph with `n` nodes. `allow-self` enables self edges.

### make-havel-hakimi

**Signature:** `(seq &key (direction :bi))`

Create a simple graph using the Havel-Hakimi algorithm with degree sequence `seq`. Node labels respect the order of `seq` (e.g., a sequence `(5 3)` produces a graph with node `0` of degree `5` and node `1` of degree `3`).

### make-random 

**Signature:** `(n p &key (allow-self nil))`

Create a uniform random graph with `n` nodes where each edge has probablity `p` of existing. 

### make-preferential

**Signature:** `(n &key (n0 0) (direction :bi) (p #'barabasi-albert))`

Create a graph with `n` nodes using the preferential attachment model. Start with a complete graph with `n0` nodes and for each new node, create edges to each existing node with a probability determined by function `p` (signature `(digraph node)`). Uses the classic Barabasi-Albert algorithm by default.

| `direction` | Description                                                       |
|-------------|-------------------------------------------------------------------|
| `:bi`       | Edges point both to and from the new node and the existing nodes. |
| `:in`       | Edges only point from the new node to existing nodes.             | 
| `:out`      | Edges only point from existing nodes to the new node.             |

### make-watts-strogatz

**Signature:** `(n k p &key (bi nil))`

Create a random graph with the Watts-Strogatz method with probability `p` of edges being re-wired. Paramters `n` and `k` match those of the `make-ring-lattice` function.

## Access

### node-property

**Signature:** `((d digraph) node key)`

Retrieve property `key` of `node`. Compatible with `setf`.

### edge-property

**Signature:** `((d digraph) begin end key)`

Retrieve property `key` of edge `begin->end`. Compatible with `setf`.

### in-degree

**Signature:** `((d digraph) node)`

Calculate the in-degree of `node`.

### out-degree

**Signature:** `((d digraph) node)`

Calculate the out-degree of `node`.

### degree

**Signature:** `((d digraph) node)`

Calculate the total degree of `node`.

### in-strength

**Signature:** `((d digraph) node weight &key (adder #'+) (initial-value nil))`

Calculate the in-strength of `node` according to `weight`, which can either be a keyword representing an edge parameter or a function with signature `((d digraph) begin end)` for calculating edge weights on-the-fly. `adder` allows you to define a custom function for combining edge weights and `initial-value` allows you to initialize the result of the calculation.

### out-strength

**Signature:** `((d digraph) node weight &key (adder #'+) (initial-value nil))`

Calculate the out-strength of `node` according to `weight`, which can either be a keyword representing an edge parameter or a function with signature `((d digraph) begin end)` for calculating edge weights on-the-fly. `adder` allows you to define a custom function for combining edge weights and `initial-value` allows you to initialize the result of the calculation.

### nodes

**Signature:** `((d digraph))`

Retrieve the list of nodes in the graph.

### edges

**Signature:** `((d digraph))`

Retrieve the list of edges in the graph, represented as `(begin . end)`.

### successor

**Signature:** `((d digraph) node)`

Retrieve the list of nodes pointed to by `node`.

### predecessor

**Signature:** `((d digraph) node)`

Retrieve the list of nodes pointing to `node`.

### node-count

**Signature:** `((d digraph))`

Retrieve the total number of nodes in `d`.

### edge-count

**Signature:** `((d digraph))`

Retrieve the total number of edges in `d`.

### node-p

**Signature:** `((d digraph) node)`

Returns `t` if `node` exists in `d`, otherwise `nil`.

### edge-p

**Signature:** `((d digraph) begin end)`

Returns `t` if edge `begin->end` exists in `d`, otherwise `nil`.

### node-property-p

**Signature:** `((d digraph) node key)`

Returns `t` if a node with property `key` exists in `d`, otherwise `nil`.

### edge-property-p

**Signature:** `((d digraph) begin end key)`

Returns `t` if an edge with property `key` exists in `d`, otherwise `nil`.

## Manipulation

### set-node

**Signature:** `((d digraph) node &rest args)`

Add a node to `d` or update its parameters. Parameters are represented in `:keyword value` pairs.

### set-edge

**Signature:** `((d digraph) begin end &rest args)`

Add an edge to `d` or update its parameters. Parameters are represented in `:keyword value` pairs.

### unset-node

**Signature:** `((d digraph) node)`

Remove a node and all edges it belongs to.

### unset-edge

**Signature:** `((d digraph) node)`

Remove an edge.

## Search

### a*

**Signature:** `((d digraph) start goal &key (weight #'weight1) (heuristic #'heuristic0) (compare #'>) (adder #'+))`

Find a path from `start` to `goal` using the popular A* search algorithm. `weight` is a keyword representing an *edge* parameter or is a function with signature `((d digraph) start end)` for advanced edge weight calculations. Similarly, `heuristic` is a keyword representing a *node* parameter or is a function with signature `((d digraph) node goal)` for advanced heuristic calculations between an arbitrary node and the goal node.

## Visualization

### dot

**Signature:** `((d digraph) &key (node-attrs nil) (edge-attrs nil))`

Generate the code for a Dotfile from graph `d`. Attributes are specified in a flat list with pairs `:attribute ((d digraph) node)` for nodes and `:attribute ((d digraph) begin end)` for edges. Eacg attribute function is applied to every node or edge in the graph to calculate attribute values.