I've been using <a href="http://git.or.cz/" title="git homepage">git</a> as my primary version control system for a number of months now.  Even though most of the projects I interact with use <a href="http://subversion.tigris.org/" title="svn homepage">Subversion</a>, I'm able to live in a git world by using <a href="http://www.kernel.org/pub/software/scm/git/docs/git-svn.html" title="git-svn manpage">git-svn</a> to pull commits from svn repositories and send commits back.  And now I'm officially part of the history of git.  By which I mean that a <a href="http://repo.or.cz/w/git.git?a=commit;h=c22486c9677682416ae6ad3ee77688e8b6923ee3" title="gitweb view of my commit">completely trivial patch</a> that I wrote has been integrated into the git source tree.

So what's so interesting about git?  Here are a few thoughts...

<strong>Speed</strong>.  I spend a lot of time working with the <a href="http://bioconductor.org/" title="Bioconductor Project Homepage">Bioconductor</a> code base which consists of 243 (and growing) contributed packages all in the same directory in an svn repository.  Performing an update (svn update) or a diff (svn diff) are both quite time consuming in this configuration.  For example, re-running 'svn up' after completing an update takes about 14 seconds on my laptop.  The equivalent operation using 'git svn rebase' takes 2 seconds.  Obtaining a complete status ('svn status') is 4s vs 2s with git (and git's diff operation is even faster).

<strong>Fast local branches</strong>.  It is very easy and fast to create local branches in your git repository where you can work on a particular feature or bug fix.  This allows me to work on a number of fixes/features at the same time without intermingling the changes; I switch from one local branch to the other and keep work separate.  This is much faster and cheaper than having multiple svn working copies.  Perhaps more important is the git-rebase command that makes it easy to maintain a series of patches (commits) on a local branch and continue to track upstream changes.

<strong>Improved log functionality</strong>.  Since the entire repository is local, log and diff in git are much much faster than svn and are still available when you are offline.  In addition, git provides a really convenient view of history that includes the commit log, the diffstat giving a summary of the files changed in the commit, and the actual patch.  This makes it possible for me to review commits across a large repository very quickly.  This log feature alone is worth using git for if you need to keep tabs on a large project.

So how do you get started?  Here are a few tips.

<strong>Install git</strong>
Git doesn't use a standard configure script.  Instead, you can find all of the options in config.mak.in.  You should copy this to config.mak and edit it to suit your needs.  On my OS X laptop I have the following in config.mak

prefix=/Users/seth/scm
NO_EXPAT=nope

Then I just 'make &#38;&#38; make install'

<strong>Install Subversion with the Perl swig bindings</strong>
I built the latest Subversion from source and followed the direction in 'subversion/bindings/swig/INSTALL' to install the Swig bindings (you will need swig).  This worked fine for me on OS X, but I have never succeeded in building the Perl bindings on our SuSE Linux servers without resorting to various tricks to add libs to the link command -- if you encounter problems, contact me and perhaps I can help.

Clone a Subversion Repository
The following command will clone a Subversion repository and set it up for use with git-svn.  What you are doing here is importing all Subversion history into a local git repository, so depending on the repository, this can take awhile.  What is surprising, is that the git repository size including all history is often the same or not much larger than an svn working copy.

  git svn clone http://someserver.com/path/to/svn/project/trunk myproj

In the resulting git repository, myproj, there should be a master branch which tracks an internal-use-only branch called remotes/git-svn.

Update the master branch to latest upstream changes

  git svn rebase

View some changes

  git log --stat -p

You can also set some configuration options and get nice colorization of various git outputs, like diff.

Warning
Git is not Subversion and follows a radically different model in that it is a distributed SCM rather than a centralized SCM.  It is really worth the time to read up a bit on git to get a sense of how it works.  It will take some getting used to.
