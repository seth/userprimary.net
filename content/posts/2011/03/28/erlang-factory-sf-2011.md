--- 
title: Erlang Factory SF 2011
kind: article
created_at: Mon Mar 28 15:00:00 -0700 2011
tags:
- erlang
- erlangfactory
---

# Erlang Factory SF 2011 Report #

Erlang Factory did not disappoint.  I've been wanting to attend an
[Erlang Factory][] conference since I started working with Erlang in
2009 and this year the stars aligned so that I could attend (thanks
[Opscode][]!).  I had a great time putting faces to names, meeting new
people, and absorbing knowledge.  Below are a few highlights from the
whirlwind.

[Opscode]: http://www.opscode.com/
[Erlang Factory]: http://www.erlang-factory.com/conference/SFBay2011

## Better Erlang Programming through Toolistry ##

The conference kicked off with a keynote from Kostis Sogonas outlining
a handful of tools produced by his lab that can help you to level-up
your Erlang programming skills.  Here's the list:

1. [Dialyzer][] - *"I don't think you should be ignoring Dialyzer
   warnings; Dialyzer is never wrong"* -- Kostis Sagonas.  Dialyzer
   isn't a brand new tool. If you aren't using it, you should do
   something about that.  Choice quote from Kostis: Getting started
   looks like this:

       # annotate your source with -type and -spec as described 
       # in http://erlang.org/doc/reference_manual/typespec.html

       # build a plt database for dialyzer
       dialyzer --build_plt --apps erts kernel stdlib
       
       # compile your erl code with debug_info
       # If you're using rebar, just add the following to rebar.config
       {erl_opts, [debug_info]}.

       # run dialyzer and fix warnings
       dialyzer ebin
   
   Adding type and function specs to your code serves as useful
   documentation (which is now parsed by Edoc in the latest Erlang
   release) and has the advantage over explicit guards of not imposing
   any runtime cost.

   Finally, visiting the [Dialyzer][] website is worth a few minutes
   as there are some reports showing warnings produced by the
   in-depvelopment version of dialyzer.  For example
   [Heisenbugs](http://www.softlab.ntua.gr/dialyzer/heisenbug/) and
   [Intersection](http://www.softlab.ntua.gr/dialyzer/intersection/)
   warnings.
    
2. [TypEr][] (pdf) - You can use this to bootstrap writing type specs
   for your code.  Comes with Erlang.  Get started with `typer
   --help`.

3. [Tidier][] - Next best thing to having a code review?  Tidier
   attempts to refactor your code to improve its readability and
   performance.  I'm Looking forward to taking this for a spin.  The
   demos in the presentation showed an number of impressive automatic
   cleanups, but it remains to be seen if this is able to provide
   value for real-world cases.  In any case, certainly on my list to
   spend some time with.

4. [PropEr][] - A property based test tool ala Quviq's [QuickCheck].
   I think property based testing is what everyone is going to be
   using once they get clued in to how awesome it is.  PropEr looks
   interesting because it understands type specs and can test those
   specs automatically.  In theory, this means you can have an
   auto-generated regression test for a module which you can use to
   test a new underlying implementation.

[Dialyzer]: http://www.softlab.ntua.gr/dialyzer/
[Tidier]: http://tidier.softlab.ntua.gr/mediawiki/index.php/Main_Page
[PropEr]: https://github.com/manopapad/proper
[TypEr]: http://user.it.uu.se/~tobiasl/publications/typer.pdf
[QuickCheck]: http://www.quviq.com/

Simon Thompson presented on [Wrangler][] a refactoring tool that plugs
into Emacs (and other editors) and looks to provide some useful
automation for common tasks like function extraction, function
generalization, and renaming along with heuristics for automatically
identifying "clones" that are candidates for function extraction.

[Wrangler]: http://www.cs.kent.ac.uk/projects/forse/

John Hughes provided an energetic introduction to property based
testing via [QuickCheck mini][].  With freely available tools like
QuickCheck Mini and PropEr, the time has come to get over the hump
with property based testing.  I'm convinced there is tremendous value
to be mined out of these tools with modest investment of effort.

[QuickCheck mini]: http://www.quviq.com/downloads.htm


Dave Smith and Joe Williams presented on [Rebar][] and OTP release
handling with Rebar.  There seems to be growing momentum behind rebar
as a standard build tool for Erlang.  A few notes from the talk:

- The time is now to start installing rebar system wide and stop
  including a copy of rebar in your projects.

- You can specify `{vsn, git}` in `src/yourproj.app.src` and rebar
  will use the closest tag via git describe to label your release.
  This means you only have to manage versions via git tagging and the
  rest just happens.  Very nice.

- `rebar escriptize` will create an escript bundle for your app in the
  same way that rebar is constructed.

[Rebar]: https://github.com/basho/rebar

## Coming Soon to Erlang ##

- Erlang crash reports will have line numbers in an upcoming release.
  You will be able to obtain the current line number of a process
  using process_info.  In describing this feature, Kenneth Lundin said
  "this could be useful for debugging" and is now in the running for
  an understatement of the year award. 

- Rickard Green from the OTP team described a new native interface for
  Erlang.  Slides are not yet available and my notes are incomplete,
  but the API presented looked extremely useful and clean.  Includes
  support for calling Erlang from C, schedule long running native
  processes on either a CPU bound thread pool or an I/O bound thread
  pool.  There is a chance these features will land in R15.
  

## Wrap Up ##

There were many other interesting talks and even more great
conversations.  These notes are incomplete, but had to get something
up before day-to-day concerns took over and the details leaked out of
my head.  To the folks at Erlang Factory and all the attendees,
thank you!  I hope to make it next year.


