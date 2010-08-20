A little less than a year ago, I started managing open source packages
on my OS X laptops using [Homebrew](http://github.com/mxcl/homebrew).
Since then, I've completely stopped using Fink and macports and
happiness has ensued.  *If you are looking for a package manager for
OSX, Homebrew is what you want*.

Way back when, I
[posted](/posts/2009/09/16/installing-the-homebrew-package-manager-for-os-x/)
on how I installed Homebrew, and those instructions are a bit out of
date.  So for those interested, here's a quick way to bootstrap a new
OS X system for Homebrew and git goodness.

    sudo chown -R $USER:staff /usr/local

    curl -Lsf http://github.com/mxcl/homebrew/tarball/master | \
        tar xvz -C/usr/local --strip 1

    brew install git

    git clone http://github.com/mxcl/homebrew.git /tmp/homebrew
    mv /tmp/homebrew/.git /usr/local/
    rm -rf /tmp/homebrew

And that's it.  The basic brew commands are:

    brew search FOO
    brew info FOO
    brew install FOO
    brew uninstall FOO
    brew list

Some other useful commands include:

    # update homebrew to latest set of "formula"
    brew update

    # show which installed packages are out of date
    brew outdated

There are now a number of `brew` extensions that provide additional
functionality and serve as examples of how to bend `brew` to your
will.  Take a look at `/usr/local/Library/Contributions/examples` to
see what's available (files in this directory with a name like
brew-BAR.rb can be invoked using `brew BAR`.

Enjoy.
