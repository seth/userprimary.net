As a means of exploring <a href="http://erlang.org">Erlang</a>, I decided to write a URL shortening service similar to the services you are likely familiar with like tinyurl, bit.ly, ur.ly, and tr.im.  A URL shortener makes for a small, well-defined learning project that touches some important areas that are likely to come up in any real-world system, namely database storage and HTTP web services.  This project uses the Mnesia database for storage and a light weight framework called <a href="http://bitbucket.org/justin/webmachine/wiki/Home">webmachine</a>, which sits on top of mochiweb, for building the RESTful HTTP API.  In this post, I'll describe the module that interfaces with mnesia.

At its core, our URL shortening service needs to map a URL to an integer, keep track of this mapping, and be able to return the original URL when given the integer code at a later point in time.  The integer codes will be turned into short URLs by encoding them to make them more compact.  I'll describe how I went about that in another post.  With the core storage system in place, the web layer will provide the user interface for creating short URLs and translating them back to their original long form.

One last thing before diving in.  Surely, the project needs a name; let's call it sherl.

The sherl database module needs to support two operations:

<ol>
<li><code>get_code(Url)</code>: Given a URL, create or look up of the integer code.</li>
<li><code>get_url(Code)</code>: Given an integer code, look up the original URL or indicate that the code is unknown.</li>
</ol>

For each URL we need to store the integer code and the URL.  As a bonus, we will also store the creation time.  Mnesia makes it easy to associate an Erlang record with an Mnesia table so we'll start with the following definition:

<pre lang="erlang">
-record(url, {code, url, created}).
</pre>

Mnesia's schema definition may seem surprisingly relaxed if one is used to relational database system.  Essentially, you define a table and specify how many columns and their names and leave it at that.  You can store any Erlang term in a given table cell.

Before we can start implementing either <code>get_code</code> or <code>get_url</code>, we need some supporting code to create the mnesia schema, start and stop mnesia, and we need the ability to generate an increasing sequence of integers (an auto increment feature).

<pre lang="erlang">
-module(sherl_db).
-export([get_code/1, get_url/1, start/1, stop/0]).
-include_lib("stdlib/include/qlc.hrl").

-record(url, {code, url, created}).
-record(counter, {id = 0, ver = 1}).

start([]) ->
    start([node()]);
start(Nodes) ->
    mnesia:create_schema(Nodes),
    mnesia:start(),
    mnesia:create_table(url, [{disc_copies, Nodes},
                              {attributes, record_info(fields, url)},
                              {index, [url]}]),
    mnesia:create_table(counter, [{disc_copies, Nodes},
                                  {attributes, record_info(fields, counter)}]),
    mnesia:wait_for_tables([url, counter], 20000).

stop() ->
    mnesia:stop().

get_code(Url) -> implement_me.

get_url(Code) -> implement_me.

%% @spec next_int() -> integer()
%% @doc This is the integer sequence used to uniquely identify URLs
next_int() ->
    mnesia:dirty_update_counter(counter, id, 1).
</pre>

Let's start with start/1 that begins by creating the mnesia schema.  The mnesia:create_schema/1 function will create a directory on disk and store information about the Erlang nodes that are a part of the database.  Note that you call it before starting mnesia and that we do not give it any specifics about our data format.  Mnesia's schema are quite different from how the term is used in RDB systems.  You control the directory where mnesia writes its schema information, as well as where local disk copies of tables will be stored by specifying a flag at Erlang start time:

<pre>
-mnesia dir '"/your/dir/here"'
</pre>

I haven't found a way to set this programatically.  For most production systems I don't imagine that poses any problem, but for testing a system that uses mnesia it would be convenient to be able to control where mnesia looks for data.  So if you know of a way to do this, please comment on this post.

Next we call create_table to define our tables, one for the url record and one for our integer sequence, more on that below.  If the tables already exist, these calls will return a tagged tuple indicating an error, but no harm is done.  Finally, we call wait_for_tables because if the tables do already exist and we are using a multi-node mnesia schema, we want to be sure the latest data is available on all nodes before allowing the API of this module to be called.

Creating the integer sequence required some searching, but ends up being very simple.  I came upon <a href="http://orbitz-erlang.blogspot.com/2006/05/pastebin-design-mnesia-tables.html">this blog post</a> that had a comment from Ulf Wiger which pointed me to the dirty_update_counter/3 function.   The counter table will only contain a single row and will get updated to keep track of the integer sequence.  One minor pitfall to be aware of is that mnesia tables must have at least two columns, hence the "ver" column the counter record definition.

What?  Your still here?  Alright, then.  On to the main API.  Let's look at get_url first because it is simpler.  I decided that both get_url and get_code should return a url record as this will keep all of the pertinent information together for clients.

<pre lang="erlang">
%% @spec get_url(integer()) -> recUrl() | undefined
%% @type recUrl() = #url
%% @doc Return the the url record for the URL associated with the
%% specified integer ID.  The atom undefined is returned if the
%% specified URL is not found in the database.
get_url(Code) ->
    F = fun() ->
                Q = qlc:q([X || X <- mnesia:table(url), X#url.code =:= Code ]),
                case qlc:e(Q) of
                    []    -> undefined;
                    [Rec] -> Rec
                end
        end,
    {atomic, Val} = mnesia:transaction(F),
    Val.
</pre>

The get_url function uses the qlc module's list comprehension syntax to query for the url record with a matching code.  For a gentle introduction to mnesia and performing queries using qlc, I highly recommend <a href="http://www.pragprog.com/titles/jaerlang/programming-erlang">Programming Erlang</a>.

Finally, we can wrap up this episode with get_code:

<pre lang="erlang">
%% @spec get_code(string()) -> recUrl()
%% @type recUrl() = #url
%% @doc Store a URL in the database and return a url record with the
%% unique integer that will be permanently associated with the URL.
%% If the URL is already in the system, the existing url record is
%% returned.
get_code(Url) ->
    F = fun() ->
                Q = qlc:q([X || X <- mnesia:table(url), X#url.url =:= Url ]),
                Rec = qlc:e(Q),
                case Rec of
                    [] ->
                        Code = next_int(),
                        New = #url{url = Url, code = Code, created = now()},
                        mnesia:write(New),
                        New;
                    [Found] ->
                        Found
                end
        end,
    {atomic, Val} = mnesia:transaction(F),
    Val.
</pre>

The get_code function requires a bit more logic since we need to first check whether we've already assigned an integer code for the specified URL, in which case we just return the record we find.  Otherwise, we obtain an integer code and write a new record to the database.  The check for existing and the possible write take place within the same transaction.  This should avoid the potential race condition of the same long URL being requested concurrently.  Note to self: it would be nice to come up with a way to test that.

That concludes part 1.  I realize that I haven't yet discussed the layout of the code on disk, how to build the code, nor how to test it.  I will try to work that into one of the follow up posts.

[Update: checkout <a href="http://userprimary.net/user/2009/06/18/writing-a-url-shortening-service-in-erlang-part-2/">part 2</a> which has among other things, an important bug fix to the code presented here]
