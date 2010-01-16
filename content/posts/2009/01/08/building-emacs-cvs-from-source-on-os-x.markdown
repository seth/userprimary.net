For awhile I had been using Aquamacs, but recently realized that I was fighting too hard against the customizations that Aquamacs brings to the table.  It's a great package, but I'm not interested in (most of) the enhancements.

So I switched to "<a href="http://homepage.mac.com/zenitani/emacs-e.html">Emacs.app</a>" aka Carbon Emacs Package which has been working quite well for me.  But a few folks at work have been running a CVS build of Emacs 23 and that gave me ideas...

1. Pull down the CVS emacs tree:
<pre>cvs -z3 -d:pserver:anonymous@cvs.savannah.gnu.org:/sources/emacs co emacs
</pre>2. Run configure and make like this:

<pre>cd emacs
./configure --with-ns --without-x
make install
</pre>3. The "install" is local only. When it completes, you should have a new Emacs.app directory in emacs/nextstep/Emacs.app.  You can copy that to /Applications and you should be off and running.

4. After you start Emacs.app, there is an OS X style preference pane under the main Emacs menu where you can configure the splat/open-apple/apple/command key to be Emacs meta.

This seems to be working for me, although a strange black square appears in the center of the Emacs frame when I hit ctrl-g.  Is that supposed to be there?  Hrm...

UPDATE The black square appearing on C-g is someone's interpretation of a "visible bell".  To make it go away, simply add the following to your init file:

<pre>(setq visible-bell nil)
</pre>
