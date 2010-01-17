In R, if you want to convert a string representing an integer in base 16 to an integer, then you can simply use <code>as.integer</code> as long as you prefix the string with "0x".  Here's an example:

<pre>
as.integer("0x4d2")
[1] 1234
</pre>

Originally, I had posted the following excerpt from <code>as.hexmode</code> because I found the implementation elegant and a good example of how to make use of R's functional and vectorized features.  I didn't know that <code>as.integer</code> knew how to do that.  It is documented, of course, but <code>help.search("hexadecimal")</code> does not give a hit there and led me astray.  I received a comment on the original post that clued me in.

<pre lang="C">
## from R's as.hexmode, how to convert a hexadecimal string to an int
hex_to_int = function(h) {
  xx = strsplit(tolower(h), "")[[1L]]
  pos = match(xx, c(0L:9L, letters[1L:6L]))
  sum((pos - 1L) * 16^(rev(seq_along(xx) - 1)))
}
</pre>

For another project, I needed to implement a base 62 conversion function to generate URL-safe strings for a URL shortener.  Base 62 is what you can represent easily using [a-zA-Z0-9].  I wrote the function in Erlang and when I came across the above implementation in R, it made me wonder whether there was a cleaner approach perhaps using <code>lists:map</code> instead of recursion like:

<pre lang="C">
decode([C | T], N, Acc) ->
    decode(T, N * 62, char_to_digit(C) * N + Acc);
decode([], _N, Acc) ->
    Acc.
</pre>
