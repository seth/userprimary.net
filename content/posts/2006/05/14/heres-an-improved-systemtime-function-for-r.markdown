R's current system.time function doesn't name the vector of return values.  Doing so makes it easier to understand the output. IMO, the current code has two uglies: it sets an on.exit hook and then calls on.exit() explicitly.  It also computes the elapsed time twice (once for the stdout message and once for the return value).  Another minor shortcoming is that when nested, only the outer call will print anything.

Anyhow, here's an improved version that uses tryCatch instead of on.exit.  One thing I wish was easier (maybe it is and I just don't know) is reproducing the error message.  

<pre>
timeit &lt;- function (expr, gcFirst = TRUE) 
{
    nms &lt;- c("User", "System", "Elapsed", "Sub.User", "Sub.System")
    if (!exists("proc.time")) {
        ans &lt;- rep(as.numeric(NA), 5)
        names(ans) &lt;- nms
        return(ans)
    }
    loc.frame &lt;- parent.frame()
    if (gcFirst) 
        gc(FALSE)
    expr &lt;- substitute(expr)
    time &lt;- proc.time()
    show_time &lt;- function() {
        t &lt;- proc.time() - time
        names(t) &lt;- nms
        cat("Timing stopped at:\n")
        print(t)
        t 
    }
    tryCatch(eval(expr, envir = loc.frame),
             error=function(e) {
                 msg &lt;- paste("Error in", deparse(conditionCall(e)),
                              ":", conditionMessage(e), "\n")
                 cat(msg)
             })
    show_time()
}
</pre>
