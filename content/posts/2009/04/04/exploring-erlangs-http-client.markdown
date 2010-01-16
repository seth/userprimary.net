Erlang's <code>http</code> module has a <code>request</code> method that provides a very flexible HTTP client.  For a project I've been working on, I had a need to make many (~1000) HTTP requests in as short a time period as possible.  So I've been experimenting with some of the options available in Erlang's HTTP client as well as different approaches to making parallel the requests.

Basic use of <code>http:request</code> looks like this

<pre lang="C">
application:start(inets).
UA = "your user agent here".
Url = "http://userprimary.net".
{ok, {{HttpVer, Code, Msg}, Headers, Body}} =
    http:request(get, {Url, [{"User-Agent", UA}]}, [], []).
</pre>

And here's what the return values look like:

<pre lang="C">
> {HttpVer, Code, Msg}.
{"HTTP/1.1",200,"OK"}

> Headers.
[{"connection","Keep-Alive"},
 {"date","Sun, 05 Apr 2009 00:38:05 GMT"},
 {"server","Apache"},
 {"content-length","1680"},
 {"content-type","text/html"},
 {"keep-alive","timeout=15, max=100"}]

> length(Body).
1680
</pre>

I started out with a text file containing 100 URLs, one per line and wrote three methods to request the URLs:
<ol>
<li><code>fetch_with_map</code> serial requests, uses lists:map to call http:request for each URL.</li>
<li><code>fetch_with_http_async</code> use the async option in http:request.</li>
<li><code>fetch_with_pmap</code> fetches in parallel using at most N concurrent processes</li>
</ol>

Here's a sample run for each

<pre lang="C">
$ ./run.sh 100.txt map
fetching with map
time: 5.059 secs

$ ./run.sh 100.txt async
fetching with async
time: 3.196 secs

$ ./run.sh 100.txt pmap 20
fetching with pmap
using 20 concurrent processes
time: 1.050 secs

$ ./run.sh 100.txt pmap 100
fetching with pmap
using 100 concurrent processes
time: 3.209 secs
</pre>

So concurrent requests helps, but more than 20 or so, for my test setup, degrades performance.  The async mode for <code>http:request</code> requires you to do your own batching to control the number of concurrent processes and since I did not do that, it performs equivalently to the <code>pmap</code> version with 100 processes.

To build <code>fetch_with_pmap</code>, I wrote an extension of the <code>pmap</code> function presented in <a href="http://www.pragprog.com/titles/jaerlang/programming-erlang">Programming Erlang</a> that uses at most N processes.  While I was at it, I also added a <code>pfilter</code> function.  It was interesting to see how these two factored out since mapping and filtering are quite similar.  Here's the implementation for <code>pmap/3</code>

<pre lang="C">
%% @spec pmap(F, L, N) -> list(any())
%%
%% @doc A parallelized version of lists:map/2 that executes at most N
%% concurrent processes.  This will perform much better than pmap/2
%% for long lists.
pmap(F, L, N) ->
    pmap(F, L, N, []).

pmap(_F, [], _N, Acc) ->
    ungather(Acc);
pmap(F, L, N, Acc) ->
    partition_call(F, L, N, Acc, fun pmap/4, fun do_pmap/2).

partition_call(F, L, N, Acc, Call, Gather) ->
    {L1, Rest} = if
                     N > length(L) -> {L, []};
                     true          -> lists:split(N, L)
                 end,
    Call(F, Rest, N, [Gather(F, L1)|Acc]).

ungather(Acc) ->
    lists:foldl(
      fun(L1, A1) ->
              lists:foldl(fun(L2, A2) -> [L2|A2] end, A1, lists:reverse(L1))
      end, [], Acc).

-define(MAKE_P_FUN(F, L, Eval, Gather),
        S = self(), 
        Ref = erlang:make_ref(), 
        Pids = lists:map(
                 fun(I) -> 
                         spawn(fun() -> Eval(S, Ref, F, I) end)
                 end, L),
        Gather(Pids, Ref)
       ).

do_pmap(F, L) ->
    ?MAKE_P_FUN(F, L, do_f, gather).

do_f(Parent, Ref, F, I) ->
    Parent ! {self(), Ref, (catch F(I))}.

gather([Pid|T], Ref) -> 
    receive 
        {Pid, Ref, Ret} -> [Ret|gather(T, Ref)]
    end; 
gather([], _) -> 
    []. 
</pre>

I broke out the helper methods and the macro to cut down on the duplication of code for implementing <code>pfilter</code>.  It isn't as bad as it may look, but programming in a functional language does tend to get one thinking in a different way.

All of the code for the HTTP request testing and <code>putil</code> is packaged in <a href="http://userprimary.net/software/fetcherl.tar.gz">fetcherl.tar.gz</a>.

What I didn't have time to explore is using multiple Erlang nodes to distribute the work.  I think it would be fairly easy to extend <code>pmap</code> and <code>pfilter</code> to accept a list of nodes and execute N concurrent processes concurrently on each of the specified nodes.  There is a spawn function that allows you to specify a node to run on that would provide the main guts of a solution.  While the syntax takes some getting used to along with the functional programming paradigm, I'm starting to see how Erlang makes building parallel and distributed programs much easier.

