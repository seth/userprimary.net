## intro ##

This post describes a brief exploration of skip lists to implement a
hash table (dictionary) data structure for use in R.  The result is an
R package called [rdict][] that is very rough around the edges.  If
that means anything to you, or if you'd like it to, read on.

[rdict]: http://github.com/seth/rdict

## the skinny on skip lists ##

A skip list is a data structure built on top of a linked list that can
be used to store a set of sorted keys (along with associated values if
desired).  The basic idea is to add additional forward pointers to
some nodes in the list that skip ahead and point to a node further
down in the list.  Since the nodes are sorted, one can eliminate large
portions of the list by skipping ahead when performing a search in a
fashion similar to binary search.

The clever part is how the nodes with extra forward pointers are
assigned.  Rather than requiring a regular spacing of such nodes, the
forward depth of a node is decided when it is added to the list
according to a random distribution.  The result is a data structure
that is fairly simple to implement and has, with high probability,
good performance characteristics.

If you want to learn more about skip lists, checkout wikipedia's [skip
list entry][1] and then head straight to a paper written by W Pugh,
the inventor of skip lists: [_Skip Lists: A Probabilistic Alternative
to Balanced Trees_][2] (pdf).

[1]: http://en.wikipedia.org/wiki/Skip_list
[2]: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.85.9211&rep=rep1&type=pdf

## rdict ##

[rdict][] is an R package that provides a dictionary data type
implemented using a skip list.  Here's what the API looks like right
now:
    
    > library("rdict")
    > d <- rdict_new()
    > rdict_put(d, "abc", 1:3)
    > rdict_get(d, "abc")
    [1] 1 2 3

    > aList <- list(x = TRUE, y = 3L)
    > rdict_mput(d, aList)
    > rdict_keys(d)
    [1] "x"   "y"   "abc"

    > rdict_as_list(d)
    $x
    [1] TRUE
    
    $y
    [1] 3
    
    $abc
    [1] 1 2 3
    
    > rdict_count(d)
    [1] 3

    > rdict_rm(d, "y")
    [1] TRUE

    > rdict_count(d)
    [1] 2


For loading a large number of elements, the rdict hash table is faster than R's
environments.  Consider the following example using a list of one
million elements:

    system.time({
         e <- new.env(parent=emptyenv(), hash=TRUE)
         list2env(ex1, envir=e)
    })
       user  system elapsed 
     28.202   0.260  29.543 
    
    system.time({
         d <- rdict_new()
         rdict_mput(d, ex1)
    })
       user  system elapsed 
      2.547   0.099   2.812 

The performance for accessing elements makes for a less impressive
comparison:

    > set.seed(0x5562)
    > kk <- sample(names(ex1))
    > system.time(for (k in kk) junk <- e[[k]])
       user  system elapsed 
     27.527   0.184  28.595 
    > system.time(for (k in kk) junk <- rdict_get(d, k))
       user  system elapsed 
     16.406   0.091  16.934 

At smaller element counts, I observed R's environment to be 2-3 times
faster for element access.  These comparisons are not entirely
surprising.  R's environment is implemented using a standard array
based hash table with collisions being handled by linked lists.  When
the hash table grows too large for the number of elements, a new table
is allocated and all elements have to be rehashed into the new table.
I suspect this explains the disparity for insert time.  The skip list
allocates a new node for each insertion, but otherwise the cost is the
same as an item search.

In many ways it is an apples and oranges comparison.  The rdict
implementation relies on all keys being part of R's internal cache of
character vector data and uses the CHARSXP memory address as the key
in the skip list.  This means no hash function is called (which can
become a factor if you have very long keys).  It also means that the
"same" string in a different encoding will be treated as a different
key.  While avoiding a hash function makes rdict faster, the rdict
code has to go out of its way to ensure that the keys and values are
protected from R's garbage collector since the skip list itself is not
implemented using R data structures.  This tracking for gc incurs
considerable overhead.

## a bug ##

Getting an initial working version of the skip list backed rdict
structure went surprisingly quickly.  I cribbed a bit from the
implementation posted [here][slimpl] and I carried a somewhat subtle
bug across in the process.  I had put, get, and remove all working,
but I noticed that as I started testing with larger element counts,
the operations were getting really, really, **really slow**.  Slow
enough that I knew I must have done something wrong.

To debug the issue, I added some tracking to the skip list
implementation so that I could inspect how many nodes of each *level*
were in the list.  Here's what I saw:

    > s$levels
       1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16 
    7057 1626  360   85   29    3    1    1    1    1    1    1    1    1    1  831 

The large number of nodes with the maximum number of levels did not look
right.  The levels are supposed to be randomly distributed such that
for any level n, there are 0.25 nodes with level n + 1.  When
inspecting the function that assigns a level based on random bits, I
found the bug.  With the fix, the list was fast and the distribution
of levels looked better:

    > s$levels
       1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16 
    7374 1961  507  106   44    6    2    0    0    0    0    0    0    0    0    0

The skip list example code has a random level function that looks like
this (original formatting preserved):

    int randomLevel()
      {register int level = 0;
       register int b;
       do {
        b = randomBits&3;
        if (!b) level++;
        randomBits>>=2;
        if (--randomsLeft == 0) {
            randomBits = random();
            randomsLeft = BitsInRandom/2;
            };
        } while (!b);
        return(level>MaxLevel ? MaxLevel : level);
        };

The problem is that two random bits are consumed, but the random bits
left (randomsLeft) is only decremented once.  Since I had so much fun
finding that one, I decided to write this all up.

[slimpl]: ftp://ftp.cs.umd.edu/pub/skipLists/skipLists.c

## take away and loose ends ##

- If you have a need to load a large number of keys into a hash table
  in R, it may be worth it to explore some alternatives to R's
  environments.

- If you would find a more polished version of the rdict package
  useful, drop me a note.

- Programming in an environment where it is easy to explore data makes
  finding bugs easier.

- The method implemented in rdict to keep the hash table keys and
  values protected from R's gc is worth a look if you are interested
  in implementing other key/value type data structures as C extensions
  for R and want to use non-R data structures to do it.  Look at
  `rdict/src/epdb.[ch]` for a start.


