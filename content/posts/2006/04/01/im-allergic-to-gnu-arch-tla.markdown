A number of smart people I know are fans of <a href="http://www.gnu.org/software/gnu-arch/">GNU Arch</a> (aka tla).  There are also a number of projects that I follow that have adopted tla as their SCM solution (<a href="http://www.emacswiki.org/cgi-bin/wiki/PlannerMode">planner-mode</a>, <a href="http://www.emacswiki.org/cgi-bin/wiki/MuseMode">muse-mode</a>, <a href="http://moinmoin.wikiwikiweb.de/">MoinMoin</a>).

Now, I really think I have given it a chance.  I've read through tutorials and even tried using it as my personal scm for awhile.  But I find it incredibly opaque, unfriendly, and difficult to use.  I think I'm allergic to it.  Just about every time I have to interact with it I come to the edge of a cursing fit.
 
Even when I get it to do what I want, it irks me.  Grab a copy of a project, make a few changes and ask for some diffs.  First you have to figure out that you should say "tla changes --diffs".  Then you wait while it does some bizarre computation to calculate a diff.

I've been experimenting with <a href="http://git.or.cz/">git</a> and I find it much friendlier and more responsive.  Here is a comparison of time to produce a simple diff for a modest tree with two small changes:
<pre>
$ time tla changes --diffs &gt; ../editor_quickhelp.diff

real    0m7.086s
user    0m4.518s
sys     0m1.359s          
</pre>

Yikes!  Here's the same operation on the <b>same tree</b>

<pre>
$ time git diff &gt; ../fixes.diff

real    0m0.162s
user    0m0.024s
sys     0m0.083s     
</pre>

Now I probably should work harder to install baz which as I understand it has a much saner user interface.  But last time I tried to build it on my OS X laptop, it errored out.  I can't build the most recent tla either.  Maybe it senses my loathing I don't know.

My current vote is going to git.  I played with Mercurial (hg) for a bit and I really like that it is all Python based, but after giving git a whirl, it just doesn't have the same usability and quickness.  

