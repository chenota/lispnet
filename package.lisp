;;;; package.lisp

(defpackage #:lispnet
  (:use #:cl)
  (:export :set-node
           :node-property
           :set-edge
           :edge-property
           :in-degree
           :out-degree
           :degree
           :in-strength
           :out-strength
           :nodes
           :edges
           :successor
           :predecessor
           :unset-node
           :unset-edge
           :node-count
           :edge-count
           :make-digraph
           :make-empty
           :make-line
           :make-ring
           :make-ring-lattice
           :make-star
           :make-complete
           :make-havel-hakimi
           :make-random
           :barabasi-albert
           :make-preferential
           :make-watts-strogatz
           :node-p
           :edge-p
           :node-property-p
           :edge-property-p
           :a*
           :dot
           :lispnet-error
           :not-found-error
           :node-not-found-error
           :property-not-found-error
           :edge-not-found-error
           :uneven-argument-error
           :nested-hash-not-found-error
           :out-of-bounds-error
           :probability-out-of-bounds-error
           :invalid-sequence
           :not-graphical
           :invalid-arg
           :excessive-arg
           :uneven-arg))
