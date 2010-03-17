One usually encounters the concept of reentrant functions in the
context of concurrent programming.  Wikipedia provides [this][1]
summary of a reentrant function:

> A computer program or routine is described as reentrant if it can be
> safely called again before its previous invocation has been
> completed (i.e it can be safely executed concurrently).

[1]: http://en.wikipedia.org/wiki/Reentrant_(subroutine)

It turns out that there are situations where one needs reentrant
functions in a program whose execution contains no concurrency.  I
recently had the opportunity to "discover" this idea while
investigating a bug in R that resulted in spurious error messages like
the one shown below.

    Error in get("disabled", envir = .validity_options) :
    formal argument "envir" matched by multiple actual arguments

This error message comes from a function called `matchArgs` in the R
internals that is responsible for aligning the formal arguments of a
function with those supplied in a particular call.  Even though R is
not multi-threaded, the `matchArgs` function needs to be reentrant.
To explain why, we have to digress into a few details about the R
system.

Memory for R objects is managed by a garbage collector that can be
triggered whenever a function that allocates memory for R objects is
called.  R also provides a mechanism to register _finalizers_ for
environments and external pointers (the two data types that have
reference semantics, see `help(reg.finalizer)` for more details).
These finalizers are executed when the given environment or external
pointer is garbage collected; one step in the garbage collection
process is to run any registered finalizers.  Still with me?

So here's the rub with `matchArgs`.  In a couple of places the
`matchArgs` code needs to allocate space for new R objects.  When it
calls an allocating function, there is the possibility that the
garbage collector will be triggered.  If the garbage collector is
triggered, an object with a finalizer might be collected.  In this
case, that object's finalizer will be run.  The finalizer can execute
arbitrary R code.  In particular, it can call an R function.  But
calling an R function requires that the system aligns the formal
arguments of the function to those supplied in the call -- the job of
`matchArgs`.  So even though the execution is not multi-threaded, the
`matchArgs` function gets called concurrently.  That is, a call to
`matchArgs` can occur in the middle of a `matchArgs` call (whenever a
garbage collection could be triggered).

The take away is to pay close attention to systems that involve
callback mechanisms, like custom finalization during garbage
collection.  Such systems can have surprising execution paths in which
a function can be called in the middle of itself without an explicit
recursive call.  Oh yeah, your code will be better if you don't use
global variables.

If you've made it this far and are curious about the actual details of
the bug found in `matchArgs` and the fix you should probably have your
head examined.  After that, you can read [this post][2] that describes
the symptom of the bug in a reproducible fashion and then look at
r51232 in the R sources.

[2]: https://stat.ethz.ch/pipermail/bioc-sig-sequencing/2010-March/000997.html
