First of all, let's be clear on who is doing the learning here.  I'm
just getting started with <a href="http://erlang.org/">Erlang</a>.  Keep that in mind if you keep
reading.

String manipulation is a good place to start digging into a new
programming language because the concepts involved are familiar, it is
easy to verify examples, and because it is useful.  Parsing, pasting,
and searching strings are all tasks that come up in just about any
programming project.

So to get started with Erlang, I decided to write some functions to
parse a URL into its constituent parts: scheme, host, port, path, and
parameters.  As a first step, I wanted a function that would parse a
URL and return a tuple containing the scheme and the rest of the URL.
For example, given the URL "http://userprimary.net", the function
should return {"http", "userprimary.net"}.  Here's a first attempt
using function's from Erlang's string module:

<pre lang="ruby" colla="+">
%% This is Erlang, not Ruby
%% The wordpress plugin I'm using doesn't know how to
%% deal with Erlang yet.
parse_scheme(Url) ->
    Pos = string:str(Url, "://"),
    if
       Pos > 1, length(Url) > (Pos + 3) ->
            {string:substr(Url, 1, Pos - 1), string:substr(Url, Pos + 3)};  
       true ->
            {bad_scheme, Url}
    end.
</pre>

Before going further, I should mention that the scheme in a URL
is delimited by the first ':' and that the '//' is part of the
scheme-specific-part.  That isn't convenient for my use case.  If that
troubles you, reach for the copy of <a href="http://www.ietf.org/rfc/rfc1738.txt">RFC-1738</a> that is clearly close at
hand and code up your own Erlang functions.  Moving on.

The above parse_scheme function should look at least vaguely familiar to the
programmers in the audience (and any non-programmers who are still
reading this, sorry).  Erlang's string module provides many standard
functions for operating on strings and you can accomplish a fair bit
without straying too far from familiar programming paradigm
territory.  But where's the fun in that?

Erlang's pattern matching, function overloading, and list handling
suggest another implementation that is, perhaps, more in the spirit of
the language (but really, what do I know):

<pre lang="ruby" colla="+">
parse_scheme2(Url) ->
    parse_scheme2(Url, []).
parse_scheme2([], Acc) ->
    {bad_scheme, lists:reverse(Acc)};
parse_scheme2("://" ++ Rest, Acc) when length(Rest) > 0 ->
    {lists:reverse(Acc), Rest};
parse_scheme2([C|Rest], Acc) ->
    parse_scheme2(Rest, [C|Acc]).
</pre>

Whew.  Not exactly more concise, but when you unpack the syntax a bit
it isn't entirely inelegant.  There are actually two function
definitions above.  In Erlang, a function is uniquely identified by
its name and its <em>arity</em>, the number of arguments in its
signature.  So above there is a definition for parse_scheme2/1 and
parse_scheme2/2.

The single argument version is a convenience wrapper that simply calls
parse_scheme2/2 with a new empty list to use as an accumulator.  The
two argument version is defined in three parts, each defining a
pattern for the input arguments.  When called, the first definition
that matches the current arguments is evaluated.  Since strings in
Erlang are really just lists of integers, parse_scheme2/2 unpacks the
input URL a character at a time and adds it to the accumulator list --
this is what the last definition does.  Since the accumulated
characters are added to the head of the list, they end up in reverse
order.

When the input starts with "://" (middle definition), we know that
we've accumulated the complete scheme and we call lists:reverse to
correct the order before returning.  The first definition covers the
case when the input is empty or doesn't contain "://".

So which definition should one use, parse_scheme or parse_scheme2?  My
initial reaction is that parse_scheme is shorter and easier to read,
but I wonder if I will feel the same way after writing (and reading) a
good bit more Erlang code.  Out of curiosity, and not because I think
it will matter for my intended use, I did a quick benchmark of the two
functions.

<pre lang="ruby" colla="+">
for(N, N, F) -> [F()];
for(I, N, F) -> [F()|for(I+1, N, F)].
timeit(F, N) -> 
    statistics(runtime),
    for(1, N, F),
    {_, RTime} = statistics(runtime),
    io:format("iteration time: ~p microseconds~n", [RTime * 1000 / N]).
</pre>

<pre>
urlutil:timeit(fun() -> urlutil:parse_scheme("http://foo") end, 10000).
iteration time: 5.0

urlutil:timeit(fun() -> urlutil:parse_scheme2("http://foo") end, 10000).
iteration time: 3.0
</pre>

So the accumulator based version does seem to run <em>slightly</em>
faster.  Now I just need to figure out what to do with the 2.0
microseconds I just saved.
