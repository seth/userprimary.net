---
title: "A Rebar Plugin for Locking Deps: Reproducible Erlang Project Builds For Fun and Profit"
kind: article
created_at: Thu Dec 26 16:20:00 -0000 2013
slug: If you use rebar to generate an OTP release project and want to have reproducible builds, you need the rebar_lock_deps_plugin plugin.
tags:
- erlang
---

## What's this lock-deps of which you speak?

If you use [rebar][] to generate an OTP release project and want to
have reproducible builds, you need the [rebar_lock_deps_plugin][]
plugin. The plugin provides a `lock-deps` command that will generate a
`rebar.config.lock` file containing the complete flattened set of
project dependencies each pegged to a git SHA.  The lock file acts
similarly to Bundler's `Gemfile.lock` file and allows for reproducible
builds (*).

Without `lock-deps` you might rely on the discipline of using a tag
for all of your application's deps. This is insufficient if any dep
depends on something not specified as a tag. It can also be a problem
if a third party dep doesn't provide a tag. Generating a
`rebar.config.lock` file solves these issues. Moreover, using
`lock-deps` can simplify the work of putting together a release
consisting of many of your own repos. If you treat the master branch
as shippable, then rather than tagging each subproject and updating
`rebar.config` throughout your project's dependency chain, you can
run `get-deps` (without the lock file), `compile`, and re-lock at the
latest versions throughout your project repositories.

The reproducibility of builds when using `lock-deps` depends on the
SHAs captured in `rebar.config.lock`. The plugin works by scanning the
cloned repos in your project's `deps` directory and extracting the
current commit SHA. This works great until a repository's history is
rewritten with a force push. If you really want reproducible builds,
you need to not nuke your SHAs and you'll need to fork all third party
repos to ensure that someone else doesn't screw you over in this
fashion either. If you make a habit of only depending on third party
repos using a tag, assume that upstream maintainers are not completely
bat shit crazy, and don't force push your master branch, then you'll
probably be fine.

[rebar]: https://github.com/rebar/rebar
[rebar_lock_deps_plugin]: https://github.com/seth/rebar_lock_deps_plugin

## Getting Started

Install the plugin in your project by adding the following to your
`rebar.config` file:

    %% Plugin dependency
    {deps, [
    	{rebar_lock_deps_plugin, ".*",
         {git, "git://github.com/seth/rebar_lock_deps_plugin.git", {branch, "master"}}}
    ]}.

    %% Plugin usage
    {plugins, [rebar_lock_deps_plugin]}.

To test it out do:

    rebar get-deps
    # the plugin has to be compiled so you can use it
    rebar compile
    rebar lock-deps

If you'd like to take a look at a project that uses the plugin, take a
look at [CHEF's](http://www.getchef.com) [erchef][] project.

[erchef]: https://github.com/opscode/erchef

## Bonus features

If you are building an OTP release project using `rebar generate` then
you can use `rebar_lock_deps_plugin` to enhance your build experience
in three easy steps.

1. Use `rebar bump-rel-version version=$BUMP` to automate the process
   of editing `rel/reltool.config` to update the release version. The
   argument `$BUMP` can be `major`, `minor`, or `patch` (default) to
   increment the specified part of a semver `X.Y.Z` version. If
   `$BUMP` is any other value, it is used as the new version
   verbatim. Note that this function rewrites `rel/reltool.config`
   using `~p`. I check-in the reformatted version and maintain the
   formatting when editing. This way, the general case of a version
   bump via `bump-rel-version` results in a minimal diff.

2. Autogenerate a [change summary commit message][] for all project
   deps. Assuming you've generated a new lock file and bumped the
   release version, use `rebar commit-release` to commit the changes
   to `rebar.config.lock` and `rel/reltool.config` with a commit
   message that summarizes the changes made to each dependency between
   the previously locked version and the newly locked version. You can
   get a preview of the commit message via `rebar log-changed-deps`.

3. Finally, create an annotated tag for your new release with `rebar
   tag-release` which will read the current version from
   `rel/reltool.config` and create an annotated tag named with the
   version.

[change summary commit message]: https://github.com/opscode/erchef/commit/dc96bd146011a9f122c87408ed2c1ae212e6627a

## The dependencies, they are ordered

Up to version 2.0.1 of `rebar_lock_deps_plugin`, the dependencies in
the generated lock file were ordered alphabetically. This was a
side-effect of using `filelib:wildcard/1` to list the dependencies in
the top-level `deps` directory. In most cases, the order of the full
dependency set does not matter. However, if some of the code in your
project uses parse transforms, then it will be important for the parse
transform to be compiled and on the code path before attempting to
compile code that uses the parse transform.

This issue was recently discovered by a colleague who ran into build
issues using the lock file for a project that had recently integrated
[lager][] for logging. He came up with the idea of maintaining the
order of deps as they appear in the various `rebar.config` files along
with a prototype patch proving out the idea. As of
`rebar_lock_deps_plugin` 3.0.0, the `lock-deps` command will (mostly)
maintain the relative order of dependencies as found in the
`rebar.config` files.

The "mostly" is that when a dep is shared across two subprojects, it
will appear in the expected order for the first subproject (based on
the ordering of the two subprojects). The deps for the second
subproject will not be in strict `rebar.config` order, but the
resulting order should address any compile-time dependencies and be
relatively stable (only changing when project deps alter their deps
with larger impact when shared deps are introduced or removed).

[lager]: https://github.com/basho/lager

## Digression: fun with dependencies

There are times, as a programmer, when a real-world problem looks like
a text book exercise (or an interview whiteboard question). Just the
other day at work we had to design some manhole covers, but I digress.

Fixing the order of the dependencies in the generated lock file is
(nearly) the same as finding an install order for a set of projects
with inter-dependencies. I had some fun coding up the text book
solution even though the approach doesn't handle the constraint of
respecting the order provided by the `rebar.config` files. Onward
with the digression.

We have a set of "packages" where some packages depend on others and
we want to determine an install order such that a package's
dependencies are always installed before the package. The set of
packages and the relation "depends on" form a directed acyclic graph
or [DAG][]. The topological sort of a DAG produces an install order
for such a graph. The ordering is not unique. For example, with a
single package C depending on A and B, valid install orders are
[A, B, C] and [B, A, C].

To setup the problem, we load all of the project dependency
information into a proplist mapping each package to a list of its
dependencies extracted from the package's `rebar.config` file.

    read_all_deps(Config, Dir) ->
        TopDeps = rebar_config:get(Config, deps, []),
        Acc = [{top, dep_names(TopDeps)}],
        DepDirs = filelib:wildcard(filename:join(Dir, "*")),
        Acc ++ [
         {filename:basename(D), dep_names(extract_deps(D))}
         || D <- DepDirs ].

Erlang's standard library provides the [digraph][] and
[digraph_utils][] modules for constructing and operating on directed
graphs. The `digraph_utils` module includes a `topsort/1` function
which we can make use of for our "exercise". The docs say:

> Returns a topological ordering of the vertices of the digraph Digraph
> if such an ordering exists, false otherwise. For each vertex in the
> returned list, there are no out-neighbours that occur earlier in the
> list.

To figure out which way to point the edges when building our graph,
consider two packages A and B with A depending on B. We know we want
to end up with an install order of [B, A]. Rereading the `topsort/1`
docs, we must want an edge `B => A`. With that, we can build our DAG
and obtain an install order with the topological sort:

    load_digraph(Config, Dir) ->
        AllDeps = read_all_deps(Config, Dir),
        G = digraph:new(),
        Nodes = all_nodes(AllDeps),
        [ digraph:add_vertex(G, N) || N <- Nodes ],
        %% If A depends on B, then we add an edge A <= B
        [ 
          [ digraph:add_edge(G, Dep, Item)
            || Dep <- DepList ]
          || {Item, DepList} <- AllDeps, Item =/= top ],
        digraph_utils:topsort(G).
    
    %% extract a sorted unique list of all deps
    all_nodes(AllDeps) ->
        lists:usort(lists:foldl(fun({top, L}, Acc) ->
                                        L ++ Acc;
                                   ({K, L}, Acc) ->
                                        [K|L] ++ Acc
                                end, [], AllDeps)).

The `digraph` module manages graphs using ETS giving it a convenient
API, though one that feels un-erlang-y in its reliance on
side-effects.

The above gives an install order, but doesn't take into account the
relative order of deps as specified in the `rebar.config` files. The
solution implemented in the plugin is a bit less fancy, recursing over
the deps and maintaining the desired ordering. The only tricky bit
being that shared deps are ignored until the end and the entire
linearized list is de-duped which required a . Here's the code:

    order_deps(AllDeps) ->
        Top = proplists:get_value(top, AllDeps),
        order_deps(lists:reverse(Top), AllDeps, []).
    
    order_deps([], _AllDeps, Acc) ->
        de_dup(Acc);
    order_deps([Item|Rest], AllDeps, Acc) ->
        ItemDeps = proplists:get_value(Item, AllDeps),
        order_deps(lists:reverse(ItemDeps) ++ Rest, AllDeps, [Item | Acc]).
    
    de_dup(AccIn) ->
        WithIndex = lists:zip(AccIn, lists:seq(1, length(AccIn))),
        UWithIndex = lists:usort(fun({A, _}, {B, _}) ->
                                         A =< B
                                 end, WithIndex),
        Ans0 = lists:sort(fun({_, I1}, {_, I2}) ->
                                  I1 =< I2
                          end, UWithIndex),
        [ V || {V, _} <- Ans0 ].

[DAG]: http://en.wikipedia.org/wiki/Directed_acyclic_graph
[digraph]: http://www.erlang.org/doc/man/digraph.html
[digraph_utils]: http://www.erlang.org/doc/man/digraph_utils.html

## Conclusion and the end of this post

The great thing about posting to your blog is, you don't have to have
a proper conclusion if you don't want to.
