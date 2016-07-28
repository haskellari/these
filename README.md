These &mdash; an either-or-both data type
====================================

[![Build Status](https://secure.travis-ci.org/isomorphism/these.svg)](http://travis-ci.org/isomorphism/these)


The type `These a b` represents having either a value of type `a`, a value of type `b`, or values of both `a` and `b`:

```haskell
data These a b = This a | That b | These a b
```

This is equivalent to `Either (a, b) (Either a b)`. Or equivalent to `Either a (b, Maybe a)`. Or various other equally equivalent types. In terms of "sum" and "product" types, `These a b` is `a + b + ab` which can't be factored cleanly to get a type that mentions `a` and `b` only once each.

The fact that there's no single obvious way to express it as a combination of existing types is one primary motivation for this package.

A variety of functions are provided in `Data.These` akin to those in `Data.Either`, except somewhat more numerous on account of having more cases to consider. Most should be self-explanatory if you're already familiar with the similarly-named functions in `Data.Either` and `Data.Maybe`.

`here` and `there` are traversals over elements of the same type, suitable for use with `Control.Lens`. This has the dramatic benefit that if you're using `lens` you can ignore the dreadfully bland `mapThis` and `mapThat` functions in favor of saying `over here` and `over there`.


Align &mdash; structural unions
==========================

There is a notion of "zippy" `Applicative`s where `liftA2 (,)` behaves like `zip` in the sense that if the `Functor` is regarded as a container with distinct locations, each element of the result is a pair of the values that occupied the same location in the two inputs. For this to be possible, the result can only contain values at locations where both inputs also contained values. In a sense, this is the intersection of the "shapes" of the two inputs.

In the case of the `zip` function itself, this means the length of the result is equal to the length of the shorter of the two inputs.

On many occasions it would be more useful to have a "zip with padding", where the length of the result is that of the *longer* input, with the other input extended by some means. The best way to do this is a recurring question, having been asked [at](http://stackoverflow.com/q/21349408/157360) [least](http://stackoverflow.com/q/22403029/157360) [four](http://stackoverflow.com/q/3015962/157360) [times](http://stackoverflow.com/q/9198410/157360) on Stack Overflow. 

Probably the most obvious general-purpose solution is use `Maybe` so that the result is of type `[(Maybe a, Maybe b)]`, but this forces any code using that result to consider the possibility of the list containing the value `(Nothing, Nothing)`, which we don't want.

The type class `Align` is here because `f (These a b)` is the natural result type of a generic "zip with padding" operation--i.e. a structural union rather than intersection. 

I believe the name "Align" was borrowed from [a blog post by Paul Chiusano](http://pchiusano.blogspot.com/2010/06/alignable-functors-typeclass-for-zippy.html), though he used `Alignable` instead.


Unalign
-------

`unalign` is to `align` as `unzip` is to `zip`. The `Unalign` class itself does nothing, as `unalign` can be defined for any `Functor`; an instance just documents that `unalign` behaves properly as an inverse to `align`.

Crosswalk
---------

`Crosswalk` is to `Align` as `Traversable` is to `Applicative`. That's really all there is to say on the matter.


Bicrosswalk
-----------

```
<cmccann> elliott, you should think of some more instances for Bicrosswalk one of these days
<shachaf> cmccann: Does it have any instances?
<elliott> cmccann: unfortunately it is too perfect an abstraction to be useful.
```

ChronicleT &mdash; a.k.a. These as a monad
=====================================

`These a` has an obvious `Monad` instance, provided here in monad transformer form.

The expected use case is for computations with a notion of fatal vs. non-fatal errors, like a hybrid writer/exception monad. While running successfully a computation carries a "record" of type `c`, which accumulates using a `Monoid` instance (as with the writer monad); if a computation fails completely, the result is its record up to the point where it ended.

A more specific example would be something like parsing ill-formed input with the goal of extracting as much as you can and throwing out anything you can't interpret.



