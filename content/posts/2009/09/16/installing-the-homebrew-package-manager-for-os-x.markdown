**Update Aug-2010**: This post is now a bit out of date.  If you are interested
in getting started with Homebrew, checkout [my updated post][1]

[1]: /posts/2010/08/19/installing-homebrew-for-OSX/

<a href="http://github.com/mxcl/homebrew">homebrew</a> is a package management system for OS X that allows you to install various open source packages much like macports. Homebrew claims to simplify some aspects of the package mangement process, for example, by putting packages in /usr/local/Cellar/PKG and symlinking into /usr/local rather than relying on a custom package database.  Another advantage is that it aims to reuse any software already installed on your Mac instead of taking a self-contained approach, like macports, where you end up needing a custom version many libraries you already have (the duplication of which can sometimes come back to bite you).<p /> Anyhow, I&#39;m just installing it now to give it a shot.  Here&#39;s how I installed it using git archive.  I thought this two liner was worth sharing:

<pre lang="bash">
sudo chown -R `whoami`:staff /usr/local
cd ~/src
git clone git://github.com/mxcl/homebrew.git
cd homebrew
git archive --format=tar HEAD|(cd /usr/local;tar xf -)
</pre>

<strong>Update</strong>:  Actually, to get more out of homebrew, you might actually want to turn your /usr/local directory into a homebrew git repository.  So instead of the above, you really want this:

<pre lang="bash">
sudo chown -R `whoami`:staff /usr/local
cd /usr/local
git clone git://github.com/mxcl/homebrew.git .
</pre>

If you plan on contributing new formulas or otherwise tweaking homebrew, you might consider first creating a fork of the main repo and cloning that into your /usr/local.
