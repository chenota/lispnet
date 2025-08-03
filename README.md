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

#### nodep

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

#### edgep

Check if an edge exists in a graph.

```
(edgep <DiGraph> <Begin: Any> <End: Any>)
```

Returns `<EdgeExists: Bool>`

#### in-degree

Get the in degree of a node.

```
(in-degree <DiGraph> <NodeValue: Any>)
```

Returns `(values <InDegree: Integer> <NodeExists: Bool>)`

#### out-degree

Get the out degree of a node.

```
(out-degree <DiGraph> <NodeValue: Any>)
```

Returns `(values <OutDegree: Integer> <NodeExists: Bool>)`