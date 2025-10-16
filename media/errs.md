# LispNet Errors

You might think it's kind of weird to have an entire page dedicated to just errors, but Common Lisp's error system is just cool enough to justify it! 

## Hierarchy

The errors in this project follow a hierarchy starting at `lispnet-error`. The error hierarchy goes from general to specific, so for example `node-not-found-error` and `property-not-found-error` are both children of the generic `not-found-error`, which is itself a child of the even more generic `lispnet-error`. What's neat about this hierarchy is that you can easily choose what *kinds* of errors you want to listen for (e.g., I only care that something needed wasn't found, not really what that thing was).

There's not a ton of errors so it's easy to figure out the error hierarchy by taking a look at [the error definition file](../src/error.lisp).

## Slots

In Common Lisp errors are classes, which means they can have instance variables (called slots). Slots can be accessed with the `slot-value` function, which means you can programatically pick out and use information from an error. For example, I can use `(slot-value err 'node)` to retrieve the label of the node that caused a `node-not-found-error` to be thrown.

## Printing

LispNet has its own error printing format which looks like `lispnet: <message> [<slot1>=<value1>,<slot2>=<value2>,...]`. Common Lisp has excellent class introspection capabilties via the metaobject protocol, which allows for the slot-value list to be automatically generated. All errors which inherit from `lispnet-error` are automatically printed this way.