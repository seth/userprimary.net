Intro
-----

This week I had a chance to make a small speed improvement to
[R](http://r-project.org) by moving some code for the commonly used
`which()` function from R into C.  Patrick, a colleague at work,
suggested the change after observing that `which` could become a
bottleneck when dealing with vectors containing millions of elements.
If you aren't familiar with R, the `which` function returns the
indices of a logical vector that contain a `TRUE` value.  The
simplicity of `which` makes this a good case study for understanding
the mechanism of R's internal functions, those that are coded in C
instead of in R itself, and to see what's involved in moving (part of)
a function from R into C.

The case for optimizing
-----------------------

It doesn't always make sense to move code from R into C.  Adding more
C code can increase the complexity of the code base and decrease the
pool of potential maintainers.  A function is a good candidate for
becoming internal if doing so results in a significant speed gain and
if the function is used in a way that this speed gain will be likely
to be appreciated.  Since `which` is a fairly low level function used
in a lot of code, speed improvements could be worth while.  Before
going further, let's take a look at the implementation of `which` in
R-2.10.1:

    which <- function(x, arr.ind = FALSE)
    {
        if(!is.logical(x))
            stop("argument to 'which' is not logical")
        wh <- seq_along(x)[x & !is.na(x)]
        dl <- dim(x)
        if (is.null(dl) || !arr.ind) {
            names(wh) <- names(x)[wh]
        }
        else { ##-- return a matrix  length(wh) x rank
        ## ... omitted
        }
        wh
    }

The core of the function is a single line packed with vectorized goodness:

    wh <- seq_along(x)[x & !is.na(x)]
    
This says, generate a sequence of integers from 1 to the length of x,
then subset it by a logical vector consisting of the elements of x
where x is not a missing (NA) value.  It breaks down into these five
operations:

1. `is.na(x)` makes one pass through x, returning another logical
   vector with the same length of x indicating which elements are NA

2. `!` makes one pass through the `is.na(x)` result returning the
   logical not of each element.
   
3. `&` makes a pass through x and `!is.na(x)` performing logical and

4. `seq_along(x)` generates a sequence of integers from 1 to
   length(x).  The result has length(x), but it doesn't need to read
   through x.

5. `[` subsetting, makes a pass through the index argument.


So four of the five operations require a full scan through the input
vector and all five require an allocation of a result vector the same
size as the input.  Summary: we'll probably be able to speed this up
by moving these operations into C code.

An implementation in C
----------------------

Below is an implementation in C of the core of the `which` function as
described above.  This isn't quite what is required in order for us to
wire it into R as a new .Internal call, but is a good step along the
way to show how the operation can be implemented in C using R data
structures.

    SEXP which_core(SEXP v)
    {
        SEXP v, v_nms, ans, ans_nms = R_NilValue;
        int i, j = 0, len, *buf;
    
        if (!isLogical(v))
            error(_("argument to 'which' is not logical"));
        len = length(v);
        buf = (int *) R_alloc(len, sizeof(int));
    
        for (i = 0; i < len; i++) {
            if (LOGICAL(v)[i] == TRUE) {
                buf[j] = i + 1;
                j++;
            }
        }
    
        len = j;
        PROTECT(ans = allocVector(INTSXP, len));
        memcpy(INTEGER(ans), buf, sizeof(int) * len);
    
        if ((v_nms = getAttrib(v, R_NamesSymbol)) != R_NilValue) {
            PROTECT(ans_nms = allocVector(STRSXP, len));
            for (i = 0; i < len; i++) {
                SET_STRING_ELT(ans_nms, i,
                               STRING_ELT(v_nms, INTEGER(ans)[i] - 1));
            }
            setAttrib(ans, R_NamesSymbol, ans_nms);
            UNPROTECT(1);
        }
        UNPROTECT(1);
        return ans;
    }

The `which_core` function allocates an int buffer the same length as
the input, makes a single pass through the input, and then allocates
an INTSXP of the appropriate length and copies over the values from
the buffer.  If names are present on the input, then a pass is made
over the "answer" vector to add the names.  Another approach is to
make two passes through the input, once to determine the number of
TRUE's and a second pass to fill out an answer vector.  A two-pass
approach involves less allocation and less memory overhead.  In the
tests I ran, I didn't see much difference in run time between the two
approaches.


Wiring up a new .Internal function
----------------------------------

With the core of the C implementation out of the way, the next step is
to handle the details required to add a new internal function to R.
At the R level, we will make the following replacement in the `which`
code:

    which <- function(x, arr.ind = FALSE)
     {
    -    if(!is.logical(x))
    -       stop("argument to 'which' is not logical")
    -    wh <- seq_along(x)[x & !is.na(x)]
    +    wh <- .Internal(which(x))
         dl <- dim(x)
    -    if (is.null(dl) || !arr.ind) {
    -       names(wh) <- names(x)[wh]
    -    }
    -    else { ##-- return a matrix  length(wh) x rank
    +    if (!is.null(dl) && arr.ind) {
    +        ##-- return a matrix  length(wh) x rank

In order for `.Internal(which(x))` to work, we need to add the
internal function to the table of internal functions, `R_FunTab`,
found in `src/names.c`.  As an aside, if you are looking to find the
source code for an internal R function, a good place to start grep'ing
is in `names.c`.  The entry we need to add looks like this:

    {"which",  /* function name used in .Internal call in R */
    do_which,  /* C entry point, by convention starts with "do_"
                  and take (SEXP call, SEXP op, SEXP args, SEXP env)
               */
    0,         /* This value is passed as op when called via .Internal,
                  it is used when two different R functions call the
                  same internal C function and need slightly different
                  behavior (e.g. return max vs min). */

    11,        /* Actually three digits XYZ, 11 => 011
                  X=0 => return value is visible, if x=1, effect is
                  same as calling invisible(ans) at R-level
                  
                  Y=1 indicates type of function, 1 => .Internal
                  
                  Z controls argument evaluation: 1 => evaluate args
                  before calling, 0 => don't evaluate (needed for
                  special functions).
               */
    1,         /* This is the arity of your function */
    
               /* read the code for details on the rest */
    {PP_FUNCALL, PREC_FN,	0}}

Finally, we need to implement `do_which`.  The signature looks like
this:

    SEXP attribute_hidden do_which(SEXP call, SEXP op, SEXP args, SEXP rho)
    {
        SEXP v, v_nms, ans, ans_nms = R_NilValue;
        int i, j = 0, len, *buf;
    
        checkArity(op, args);
        v = CAR(args);
        
        /* rest of code here, same as which_core */

Aside from the change in arguments, there are two changes to the code
when compared to the `which_core` version.  First, the actual
arguments are packaged in a pair-list in `args` and you can see we
access the first (and only) argument via `CAR(args)`.  Second, we
call the helper function `checkArity` that will display a standard
error message if our internal function is called with the wrong number
of arguments.

Measuring progress
------------------

[Here](http://gist.github.com/332031) is a script that checks results
returned by the C version against those from the R version and does
some timing comparisons.  Below are the timing results for a logical
vector of seven million elements as the number of TRUE elements varies
between 10 and 90 percent.

    pct  orig   new
    0.1 0.340 0.021
    0.3 0.384 0.044
    0.5 0.455 0.053
    0.7 0.435 0.053
    0.9 0.435 0.041

As you can see, the C version is an order of magnatude faster,
although it shows more variance based on the percentage of TRUE values
in the input.


