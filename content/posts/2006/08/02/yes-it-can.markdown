Joel on Software's <a href="http://www.joelonsoftware.com/items/2006/08/01.html" title="Joel on Software">latest post</a> is about having first class functions in your programming language.  He shows how you can use functions as arguments to functions to reduce code duplication.  IReducing code duplication is a nice way to motivate language features since the process of removing duplication almost always results in improved code.  

He also links first class functions to Google's <a href="http://labs.google.com/papers/mapreduce.html">MapReduce</a>.  What doesn't get mentioned is how first-class functions play with closures (lexical scope) and how powerful these notions are when put together.

You can use this in a duplication reducing way to simplify repeated function calls where only a few of the parameters change.  For example:

<pre>foo(a, b, c, "yes")
foo(a, b, c, "no")
foo(a, b, c, "maybe")

myfoo <- function(a, b, c, ans) {
    function(ans) {
        foo(a, b, c, ans)
    }
}

myfoo("yes")
myfoo("no")
myfoo("maybe")</pre>

You can also use lexical scope to maintain state across function calls.  This one is perhaps less interesting since in most OO languages, this sort of thing is done via classes.

<pre>
callCounter <- function() {
    count <- 0
    function() {
        print(paste("I've been called", count, "times"))
        count <<- count + 1
    }
}


countCalls <- callCounter()
> countCalls()
[1] "I've been called 0 times"
> countCalls()
[1] "I've been called 1 times"
> countCalls()
[1] "I've been called 2 times"
> 
</pre>
