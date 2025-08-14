# lispnet
### _Alex Chenot_

LispNet is a network library for Common Lisp.

## DiGraph

`DiGraph` is a class that represents a directed graph.

### Constructor

Use the `make-digraph` function to create a new `DiGraph` instance. This function does not take any arguments.

```
(make-digraph)
```

### Methods

#### set-node

Update an the properties of an existing node or create a new node with properties.

```
(set-node <DiGraph> <NodeValue: Any> &rest <Key1: Keyword> <Value1: Any> ...)
```

Returns `nil`

#### node-property

Retrieve a property value of a node.

```
(node-property <DiGraph> <NodeValue: Any> <Key: Keyword>)
```

Returns `(values <Value: Any> <NodeExists: Bool> <PropertyExists: Bool>)`

#### setf node-property

Set a property value of a node; this function is effectively a wrapper of `set-node`.

```
(setf (node-property <DiGraph> <NodeValue: Any> <Key: Keyword>) <Value: Any>)
```

Returns `<Value: Any>`

#### node-p

Check if a node exists in a graph.

```
(nodep <DiGraph> <NodeValue: Any>)
```

Returns `<NodeExists: Bool>`

#### set-edge

Update the properties of an existing edge or create a new edge with properties.

```
(set-edge <DiGraph> <Begin: Any> <End: Any> &rest <Key1: Keyword> <Value1: Any> ...)
```

Returns `nil`

#### edge-property

Retrieve a property value of an edge.

```
(edge-property <DiGraph> <Begin: Any> <End: Any> <Key: Keyword>)
```

Returns `(values <Value: Any> <EdgeExists: Bool> <PropertyExists: Bool>)`

#### setf edge-property

Set a property value of an edge; this function is effectively a wrapper of `set-edge`

```
(setf (edge-property <DiGraph> <Begin: Any> <End: Any> <Key: Keyword>) <Value: Any>)
```

Returns `<Value: Any>`

#### edge-p

Check if an edge exists in a graph.

```
(edgep <DiGraph> <Begin: Any> <End: Any>)
```

Returns `<EdgeExists: Bool>`

#### in-degree

Get the in-degree of a node.

```
(in-degree <DiGraph> <NodeValue: Any>)
```

Returns `(values <InDegree: Integer> <NodeExists: Bool>)`

#### out-degree

Get the out-degree of a node.

```
(out-degree <DiGraph> <NodeValue: Any>)
```

Returns `(values <OutDegree: Integer> <NodeExists: Bool>)`

#### in-strength

Get the in-strength of a node by a specific property key. This function works a lot like the reduce function and allows you to specify an operation to combine property values; the operation signature is `<Type of Init>*<Type of Result>-><Type of Result>`.

```
(in-strength <DiGraph> <NodeValue: Any> <Key: Keyword> &key <Operation: Any*Any->Any, Default #'+> <Init: Any, Default nil>)
```

Returns `(values <InStrength: Any> <NodeExists: Bool>)`

#### out-strength

Get the out-strength of a node by a specific property key. This function works a lot like the reduce function and allows you to specify an operation to combine property values; the operation signature is `<Type of Init>*<Type of Result>-><Type of Result>`.

```
(out-strength <DiGraph> <NodeValue: Any> <Key: Keyword> &key <Operation: Any*Any->Any, Default #'+> <Init: Any, Default nil>)
```

Returns `(values <OutStrength: Any> <NodeExists: Bool>)`

#### nodes

Get a list of all node values in the network.

```
(nodes <DiGraph>)
```

Returns `(values <Nodes: Any List> <NodeExists: Bool>)`

#### successor

Get all successors of a node in the network.

```
(successor <DiGraph> <NodeValue: Any>)
```

Returns `(values <Successors: Any List> <NodeExists: Bool>)`

#### predecessor

Get all predecessors of a node in the network.

```
(predecessor <DiGraph> <NodeValue: Any>)
```

Returns `(values <Predecessors: Any List> <NodeExists: Bool>)`

#### node-property-p

Check if a node has a certain property.

```
(node-property-p <DiGraph> <NodeValue: Any> <Key: Keyword>)
```

Returns `(values <HasProperty: Bool> <NodeExists: Bool>)`

#### edge-property-p

Check if an edge has a certain property.

```
(edge-property-p <DiGraph> <Begin: Any> <End: Any> <Key:Keyword>)
```

Returns `(values <HasProperty: Bool> <EdgeExists: Bool>)`

#### rem-node

Remove a node from the graph.

```
(rem-node <DiGraph> <NodeValue: Any>)
```

Returns `<NodeExisted: Bool>`

#### rem-edge

Remove an edge from the graph.

```
(rem-edge <DiGraph> <Begin: Any> <End: Any>)
```

Returns `<EdgeExisted: Bool>`