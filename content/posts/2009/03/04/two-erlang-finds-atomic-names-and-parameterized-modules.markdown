I just watched <a href="http://beebole.com/blog/erlang/tutorial-web-application-erlang/">this video</a> on beebole.com demonstrating how to use <a href="http://code.google.com/p/mochiweb/">mochiweb</a> to build a web application in Erlang using mnesia as the database.  It's an informative screencast, but what really grabbed me was one small trick shown in the video and another described in the posted comments.

<strong>Atomic Names</strong>

In Erlang, you call the function <em>f</em> in module <em>m</em> using the syntax <em>m:f</em>.  What I learned from the video is that module and function names in Erlang are atoms and you can substitute variables containing the appropriate atom.  This allows you to call a function by name.  Here's an example:

<pre lang="C">
1> string:concat("hello ", "world").
"hello world"
2> Module = list_to_atom("string").
string
3> Func = list_to_atom("concat").
concat
4> Module:Func("like", "this").
"likethis"
</pre>

This was used in the example webapp to dispatch to a handler function based on text found in the request.

<strong>Parameterized Modules</strong>

A comment by Bob Ippolito clued me in to an experimental and undocumented feature of recent Erlang releases called <a href="http://www.erlang.se/workshop/2003/paper/p29-carlsson.pdf">parameterized modules (pdf)</a> which provide a way of encapsulating state within a module instance.  The result is a construct with some similarities to an immutable class in languages like Java with class-based object systems that bundle data and functions that operate on the data into single object.

A parameterized module allows you to instantiate a module with a list of values and then access those values from any function within the module.  Parameterized modules can only be used from other modules, not via the Erlang shell.  In the example below, talk.erl is the parameterized module and main.erl is an example of a module using the parameterized talk module.  You declare a parameterized module by specifying a list as the second argument to 'module'.  This will automaticaly define a new/1 function for the parameterized module which clients will use to instantiate a new module instance with a specified list of data.

<pre lang="C">
%% talk.erl
-module(talk, [What]).
-export([talk/0]).

talk() ->
    io:format("~p~n", [What]).
</pre>

<pre lang="C">
%% main.erl
-module(main).
-export([start/0]).

start() ->
    Cat = talk:new("meow"),
    Dog = talk:new("woof"),
    Cat:talk(),
    Dog:talk(),
    ok.
</pre>

<pre lang="C">
$ erl
Erlang (BEAM) emulator version 5.6.5 [source] [smp:2] [async-threads:0] [kernel-poll:false]

Eshell V5.6.5  (abort with ^G)
1> c("talk").
{ok,talk}
2> c("main").
{ok,main}
3> main:start().
"meow"
"woof"
ok
</pre>

That is all.

