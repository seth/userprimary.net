I posted this as a response to a question on R-help.  I think the idea of a "collect" function could be useful both in the context of unreliable functions that sometimes error out and also in filtering contexts where currently one creates a list containing good elements and some sort of sentinel, usually NULL, which has itself to be filtered out in a separate subsetting operation after the main filtering loop.

Here's an example:

<pre>
    d &lt;- runif(20, min=-2, max=8) # test data
    
    aFunc &lt;- function(x) {  # gives error occasionally
        if (x &gt; 0)
          x
        else
          stop("encountered bad x")
    }
    

collect &lt;- function(x, FUN, skip_error=TRUE, args_list=NULL)
{
    if (!is.vector(x))
      stop("arg x must be a vector")
    fname &lt;- deparse(substitute(FUN))
    xvar &lt;- deparse(substitute(x))
    i &lt;- 1
    j &lt;- 1
    result &lt;- vector(mode=mode(x), length=length(x))
    while (i &lt;= length(x)) {
        tryCatch({
            args &lt;- list(x[i])
            if (length(args_list))
              args &lt;- c(args, args_list)
            ans &lt;- do.call(FUN, args)
            result[j] &lt;- ans
            j &lt;- j + 1
        }, error=function(e) {
            if (!skip_error) {
                msg &lt;- paste("collect\n",
                             "call to", fname, "failed at", 
                             paste(xvar, "[",  i, "]\n", sep=""),
                             "Message:\n", conditionMessage(e))
                stop(msg, call.=FALSE)
            }
            NULL
        },
                 finally={i &lt;- i + 1})
    }
    if (j &gt; 1)
      result[1:(j-1)]
    else
      vector(mode=mode(x), length=0)
}

## Example

collect(d, aFunc, skip_error=FALSE)
Error: collect
 call to aFunc failed at d[2]
 Message:
 encountered bad x

collect(d, aFunc, skip_error=TRUE)
 [1] 7.7380303 0.7554328 1.8352623 0.5136118 4.4231091 2.5368103 1.8656615
 [8] 2.9244200 2.1364120 7.6711189 0.2141325 7.8216620 5.8347576 5.3939892
</pre>
