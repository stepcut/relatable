relatable
=========

This implements a pure, strongly typed relational algebra. It is currently a proof-of-concept implementation.

The type system should prevent you from doing stupid things like projecting non-existent fields or trying to perform a selection/restriction on fields of different types.

The syntax for constructing queries is currently verbose. I expect it can be made leaner by providing a simple DSL.
