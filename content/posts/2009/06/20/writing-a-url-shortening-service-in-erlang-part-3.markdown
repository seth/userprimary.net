In this installment, we'll turn the core sherl_db module into a proper OTP application by creating a sherl gen_server, a supervisor, and a an application resource file.  While the sherl_db module can already be used as a server (the tests in <a href="/posts/2009/06/18/writing-a-url-shortening-service-in-erlang-part-2/">part 2</a> show that it can be called concurrently), setting up a gen_server will provide a standardized framework for packaging the code as well as a nice abstraction layer should we decide to replace the mnesia-based sherl_db module with something else.

First we will tackle the gen_server part of the show.  The erlang-mode for Emacs provides a very useful template for creating new gen_servers; even if you don't use Emacs, you might want to steal the template idea as it proves quite useful.  The gen_server will live in a module called simply <code>sherl</code> and in addition to the required gen_server behavior callbacks, exports the following API:

<pre lang="erlang">
-export([start_link/0, stop/0, encode/1, decode/1]).
</pre>

A client will call sherl:encode to turn a URL into a code that we will use as a short URL.  The code we will use is the base 62 encoding of an integer that the sherl_db module assigns to the given URL.  We choose base 62 as it allows us to represent short URL codes using A-Z, a-z, and 0-9 which will keep our short URLs very clean and easy to parse -- and in particular, will reserve "." for use as a format extension so that web users can append ".json" or ".xml" to retrieve the short URL meta data in a desired format.  Below is the definition for the main sherl public API:

<pre lang="erlang">
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:call(?SERVER, stop).

encode(Url) ->
    gen_server:call(?SERVER, {encode, Url}).

decode(Code) ->
    gen_server:call(?SERVER, {decode, Code}).
</pre>

While you don't have to define a wrapper API for a gen_server, it makes using it that much more convenient.  With the above API, client code can simply call <code>sherl:encode(Url)</code> instead of <code>gen_server:call(sherl, {encode, Url})</code>.

As you can see, each API call maps to a message sent as a tuple with first element describing the desired action.  The server will read the message and dispatch to the appropriate function to respond to the client's request.

Before going further with the API, a short diversion to implement the init callback.  Here it is:

<pre lang="erlang">
init([]) ->
    sherl_db:start([]),
    {ok, #state{}}.
</pre>

Next up is implementing the handle_call callback.  The is where the real work of the server gets done.  In implementing the callback code, one realizes that what is really going on is message passing.  The client sends the sherl gen_server a message and the sherl server examines the message to determine how to respond.  Here's what we need to support the API defined above:

<pre lang="erlang">
handle_call({decode, Code}, _From, State) ->
    case sherl_db:get_url(base62:decode(Code)) of
        undefined ->
            {reply, {not_found, Code}, State};
        UrlRec ->
            {reply, {ok, UrlRec#url.url}, State}
    end;
handle_call({encode, Url}, _From, State) ->
    Rec = sherl_db:get_code(Url),
    Code = base62:encode(Rec#url.code),
    {reply, {ok, Code}, State};
handle_call(stop, _From, State) ->
    sherl_db:stop(),
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.
</pre>

Our server doesn't maintain any state between calls, but if it did, we would thread the state data through all the callbacks using the State variable that you see passed in as an argument and returned by all of the callbacks.  Ok, takes care of the sherl gen_server.  Now we need a supervisor and an application module.  As these follow fairly mechanically from their required callbacks, I will simply list them below.

<pre lang="erlang">
-module(sherl_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Sherl = {sherl_main, {sherl, start_link, []},
              permanent, 2000, worker,[sherl]},
    {ok, {{one_for_one, 20, 60}, [Sherl]}}.
</pre>

<pre lang="erlang">
-module(sherl_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    case sherl_sup:start_link() of
        {ok, Pid} -> 
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.
</pre>

Finally, we need an application resource file (sherl.app) and that goes like this:

<pre lang="erlang">
%% -*- mode: erlang -*-
{application, sherl, 
 [{description, "sherl URL shortening core service"}, 
  {vsn, "0.1"}, 
  {modules, [base62, sherl, sherl_app, sherl_db, sherl_sup]}, 
  {registered, [sherl, sherl_sup]}, 
  {applications, [kernel, stdlib, mnesia]}, 
  {mod, {sherl_app, []}}, 
  {start_phases, []} 
 ]}.
</pre>

I put sherl.app in the src directory along with the *.erl files and have the build process copy it to ebin/sherl.app since application resource files need to be on your code path for the application module to find them.

We now have an OTP application.  Here's some example usage from the Erlang shell.

<pre lang="">
Eshell V5.7.1  (abort with ^G)
1> application:start(mnesia).
ok
2> application:start(sherl).
ok
3> sherl:encode("/posts/2009/06/20/writing-a-url-shortening-service-in-erlang-part-3/").
{ok,"6"}
4> sherl:decode("6").
{ok,"/posts/2009/06/20/writing-a-url-shortening-service-in-erlang-part-3/"}
5> sherl:decode("abc").
{not_found,"abc"}
6> gen_server:call(sherl, blah).
ignored
7> application:stop(sherl).

=INFO REPORT==== 20-Jun-2009::22:19:02 ===
    application: sherl
    exited: stopped
    type: temporary
ok
8> 
</pre>

In the sherl.app resource file, we listed mnesia as an application that must be running for sherl to start.  The application framework will verify that mnesia is running and error out before starting sherl if mnesia is not running.  It will not, however, start mnesia for you which is why, in the demo above, I had to explicitly start mnesia before starting sherl -- even though the sherl_db start function will start mnesia if needed.

So far we've written the core database module, sherl_db, tested it using Common Test, and packaged it into an OTP application.  In the next installment, we'll add the HTTP interface for the sherl URL shortening service using the webmachine framework.

<strong>UPDATE</strong> I realized that the initialization of sherl isn't quite right when starting for the first time.  After <a href="http://www.erlang.org/cgi-bin/ezmlm-cgi?4:mss:44846:200906:iecdocabpddjinbfojpm">posting a question</a> to the Erlang questions mailing list, I came up with the following adjustment to sherl_db.erl based on the helpful replies I received:

<pre lang="erlang">
start(Nodes) ->
    case is_fresh_startup() of
        true ->
            case mnesia:system_info(is_running) of
                yes ->
                    error_logger:info_report("stopping mnesia"),
                    mnesia:stop();
                _ -> pass
            end,
            mnesia:create_schema(Nodes),
            error_logger:info_report("mnesia schema created"),
            error_logger:info_report("starting mnesia"),
            mnesia:start(),
            mnesia:create_table(url, [{disc_copies, Nodes},
                                      {attributes, record_info(fields, url)},
                                      {index, [url]}]),
            mnesia:create_table(counter, [{disc_copies, Nodes},
                                          {attributes,
                                           record_info(fields, counter)}]),
            error_logger:info_report("mnesia tables created");
        {exists, Tables} ->
            ok = mnesia:wait_for_tables(Tables, 20000)
    end.

%% @spec is_fresh_startup() -> true | false
%% @doc Returns true if mnesia has not been initialized with
%% the sherl schema.
%% Thanks to Dale Harvey for this function posted to
%% the erlang questions mailing list.
is_fresh_startup() ->
    Node = node(),
    case mnesia:system_info(tables) of
        [schema] -> true;
        Tbls ->
            case mnesia:table_info(schema, cookie) of
                {_, Node} -> {exists, Tbls};
                _                 -> true
            end
    end.

</pre>

The issue was that since mnesia is listed as a required application in sherl.app, it  needs to be started before the sherl application can start.  But once mnesia is started, you cannot (easily) create a schema.  The approach taken above is to detect this fresh start situation and stop mnesia in order to initialize the desired schema.  Other approaches, which perhaps are more appropriate for a production environment, that were suggested on the list include using the <a href="http://jungerl.cvs.sourceforge.net/viewvc/jungerl/jungerl/lib/builder/">builder module</a> which will create a load script that could be customized or to make use of mnesia:change_config (although I believe this requires a separate running mnesia).
