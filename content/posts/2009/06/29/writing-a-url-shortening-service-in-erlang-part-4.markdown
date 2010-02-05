In this post, we will add an HTTP interface to sherl using webmachine.  The goal is a simple API that will shorten long URLs and redirect short URLs to their original page.  Sending a GET to <code>/http://long.it/is/your/url/here</code> will create a short URL and display it.  This will make the service easy to use by hand, as a user can simply prepend "http://sherl.com/" to an address in their browser's address bar.  All GET requests that don't match "/http:" will be interpreted as short URLs and will redirect (HTTP 301) to the original page or give a 404 error if the specified short code was not found in the sherl database.

The first step is to obtain the latest webmachine code.  You can find a tarball under the get source link on <a href="http://bitbucket.org/justin/webmachine/wiki/Home">webmachine's bitbucket page</a>, or you can install Mercurial and pull down the latest bits.  On OS X, here's what I did:

<pre lang="bash">
sudo port install mercurial
hg clone https://userprimary@bitbucket.org/justin/webmachine/
cd webmachine
make
</pre>

Next, create a skeleton webmachine project using the <code>new_webmachine.erl</code> script.

<pre lang="bash">
$PATH_TO_WEBMACHINE/scripts/new_webmachine.erl sherlweb .
cd sherlweb
make
./start-dev.sh
</pre>

At this point, you should have a working webmachine application running at
<a href="http://localhost:8000">http://localhost:8000/</a> that will return a <i>Hello, new world</i> message in a browser.  Webmachine maps resources to erlang modules in which you define callback functions to implement different parts of HTTP.  Webmachine brings the protocol back to web application frameworks and makes it easy to create a REST API that behaves according to the HTTP specification.  A nice <a href="http://bitbucket.org/justin/webmachine/wiki/WebmachineMechanics">overview of how webmachine works</a> is provided on the wiki.  On that page is a link to an <a href="http://bitbucket.org/justin/webmachine/wiki/BigHTTPGraph">HTTP flowchart</a> that gives a visual explanation to webmachine's approach.  The flowchart presents HTTP as a sequence of decision points covering all of the nooks and crannies of the protocol that you'd probably prefer to ignore.  A webmachine resource can define a function for each decision point to control how the request should flow through the diagram.  Fortunately, sane default functions are available for nearly all of the decision points so you only need to implement the parts of the protocol that are relevant to your application.

There are three significant benefits to the approach taken by the webmachine framework.  First, it is much easier to end up with a web application that actually behaves according to the HTTP spec.  This makes your application easier for clients to make use of and allows common pieces of web infrastructure like caches and proxies to play nice with your service.  Secondly, by breaking down a resource into side-effect free functions, you end up with code that is easy maintain and easy to test.  Finally, webmachine comes with a very clever visual debugger that displays an interactive version of the HTTP flowchart and allows you to inspect the request and decision point function output at each step.  In this post we'll just be scratching the surface of webmachine's features, but you have to start somewhere.

We'll begin customizing the skeleton by defining the resources in the sherlweb/priv/dispatch.conf file.  This file maps URLs to resource modules and is similar to the routes configuration in a Ruby on Rails application.  The dispatch configuration consists of a list of resource definitions.  Each definition is a three element tuple: a pattern describing the URLs to match, the name of a module, and initialization parameters to send to the module.

<pre lang="erlang">
%% dispatch.conf
{["http:", '*'], shorten_resource, []}.
{['*'], lookup_resource, []}.
</pre>

The first directive will match resources that start with "/http:" and dispatch them to the shorten_resource module.  The second directive matches any resource and will dispatch to the lookup_resource module.  Matching is done in order with the first match winning so the second definition acts as a catch-all.

Now we'll implement minimal versions of shorten_resource and lookup_resource.  You can use the default sherlweb_resource module as a template and replace the to_html function as follows:

<pre lang="erlang">
%% shorten_resource.erl
%% module boilerplate excluded

to_html(ReqData, State) ->
    "/" ++ LongUrl = wrq:raw_path(ReqData),
    ShortUrl = make_short_url(LongUrl, ReqData),
    {["<html><body><a href=\"", ShortUrl, "\">", ShortUrl,
      "</a></body></html>"],
     ReqData, State}.

make_short_url(LongUrl, ReqData) ->
    {ok, Code} = sherl:encode(LongUrl),
    Host = wrq:get_req_header("host", ReqData),
    "http://" ++ Host ++ "/" ++ Code.
</pre>

<pre lang="erlang">
%% lookup_resource.erl
%% module boilerplate excluded

to_html(ReqData, State) ->
    {ok, LongUrl} = sherl:decode(wrq:disp_path(ReqData)),
    {["<html><body><a href=\"", LongUrl, "\">", LongUrl,
      "</a></body></html>"],
     ReqData, State}.
</pre>

In addition, we need to update sherlweb.app by removing sherlweb_resource and adding the above two modules (also delete sherlweb_resource.erl from the src directory).  Then modify sherlweb.erl so that the sherl application implemented in <a href="/posts/2009/06/20/writing-a-url-shortening-service-in-erlang-part-3/">Part 3</a> will be started as part of the web app initialization.  The modified sherlweb:start/0 looks like this:

<pre lang="erlang">
%% from sherlweb.erl
start() ->
    sherlweb_deps:ensure(),
    ensure_started(crypto),
    ensure_started(webmachine),
    ensure_started(mnesia),
    ensure_started(sherl),
    application:start(sherlweb).
</pre>

At this point, rebuilding and restarting the project should allow you to test the basic API and see a very rudimentary HTML-based shortening service.  To refine the service, we will iterate on the lookup_resource module so that it will return a 301 redirect for a valid short URL and a 404 otherwise.  To do that we will implement the resource_exists, previously_existed, and moved_permanently callbacks.  The new version of lookup_resource.erl looks like this:

<pre lang="erlang">
-module(lookup_resource).
-export([init/1, moved_permanently/2, previously_existed/2,
         resource_exists/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(sherl, {long_url = undefined}).

init([]) -> {ok, #sherl{}}.

resource_exists(ReqData, State) ->
    {Status, LongUrl} = sherl:decode(wrq:disp_path(ReqData)),
    case Status of
        ok -> {false, ReqData, State#sherl{long_url = LongUrl}};
        _ -> {false, ReqData, State}
    end.

previously_existed(ReqData, State) ->
    case State#sherl.long_url of 
        undefined ->
            {false, ReqData, State};
        _ ->
            {true, ReqData, State}
    end.

moved_permanently(ReqData, State) ->
    case State#sherl.long_url of 
        undefined ->
            {false, ReqData, State};
        LongUrl ->
            {{true, LongUrl}, ReqData, State}
    end.
</pre>

Based on the flowchart, the resource_exists function will be called first.  It looks up the short code using sherl:decode/1 and updates the resource state record with the long URL if it is found.  The resource_exists function always returns false since we never want to return content from this resource.  We could optimize this a bit and return 404 without further decision function processing in the case that we don't have a long URL for the requested short code.  Implementing this short circuit is left as an exercise for the reader.  Next the flow will call previously_existed which returns true if we found a long URL matching the requested short code and false otherwise.  When previously_existed returns false, the server responds with a 404.  When it returns true, the flow calls moved_permanently and webmachine generates a 301 response.

At this point we have a simple, but functioning URL shortening service backed by sherl, an OTP application that usees mnesia for storage, and fronted (is that a word?) by webmachine.  This seems like as good a stopping point as any.  I'm cleaning up the code and plan on posting it on github when it's ready.

<strong>UPDATE</strong> the code for sherl is now available on github:
<a href="http://github.com/seth/sherl/tree/master">http://github.com/seth/sherl/tree/master</a>
