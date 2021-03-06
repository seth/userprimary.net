>  **Update 2010-08-01**: The format for Homebrew formulae has changed
>  a bit since this was posted.  Since `mg` is now part of the
>  official Homebrew repo, you can view its formula file as an example
>  to compare what has changed.

One of the things that I really like about
[homebrew](http://github.com/mxcl/homebrew), a new package
manager for OS X is that creating new packages is very easy if you've
done a minimal amount of Ruby programming.  Each package -- or
*formula* in homebrew parlance -- has associated with it a Ruby
file containing at least one class extending homebrew's Formula class
with the details for downloading, verifying, and installing a given
piece of software.

If you've installed homebrew into `/usr/local`, then all of the
formula files live in `/usr/local/Library/Formula`.  When making a new
formula, I've found that finding a similar example has been quite
helpful.

In the best case, one can create a new formula using the `brew create`
command and specifying the URL for the software.  In this post, I'll
demonstrate the process to create a package for the light-weight
emacs-key-binding-friendly editor
[mg](http://www.han.dds.nl/software/mg)

The first step in building a new formula for homebrew is to try
calling `brew create` with the URL for the package tarball,
in this case, the mg source tarball.

    brew create 'http://www.dds.nl/~han/software/mg/mg-20090107.tar.gz'

This will open `$EDITOR` and allow you to make any changes.  Here's
the formula file that brew created for me by default:


    require 'brewkit'
    
    class Mg < Formula
      url 'http://www.dds.nl/~han/software/mg/mg-20090107.tar.gz'
      homepage ''
      md5 ''
    
    # depends_on 'cmake'                                                                                
    
      def install
        system "./configure", "--prefix=#{prefix}", "--disable-debug", "--disable-dependency-tracking"
    #   system "cmake . #{std_cmake_parameters}"                                                        
        system "make install"
      end
    end

That looked reasonable, so I saved it (the file will live in
`/usr/local/Library/Formula/mg.rb`), closed my editor, and
tried installing it.


    [orange] ~: brew install mg
    ==> Downloading http://www.dds.nl/~han/software/mg/mg-20090107.tar.gz
    ######################################################################## 100.0%
    ==> Warning! Cannot verify package integrity
    The formula did not provide a download checksum
    For your reference the MD5 is: f25a139da44c3a2f760ffec531bd996e
    ==> ./configure --prefix=/usr/local/Cellar/mg/20090107 --disable-debug --disable-dependency-tracking
    ==> make install
    /usr/local/Cellar/mg/20090107: 4.0K, built in 2 seconds

Despite the success report, not much was actually installed.  A little
investigation showed that this was because mg's configure script is
not standard and errors out if any options are specified.  The
downloaded tarball is cached in `~/Library/Caches/Homebrew/` which
makes it less painful to make repeated attempts at getting the formula
right.  A bit more tweaking lead to the following formula file:

    require 'brewkit'
    
    class Mg <Formula
      url 'http://www.dds.nl/~han/software/mg/mg-20090107.tar.gz'
      homepage 'http://www.han.dds.nl/software/mg'
      md5 'f25a139da44c3a2f760ffec531bd996e'
    
      def install
        system "./configure"
        system "make prefix=#{prefix}"
        system "make install prefix=#{prefix}"
      end
    end

So now we try installing it again.


    [orange] ~: brew install mg
    Formula already installed: /usr/local/Cellar/mg/20090107

Oh, well, that's easy enough to deal with:

    [orange] ~: rm -rf /usr/local/Cellar/mg/20090107

    [orange] ~: brew install mg
    ==> Downloading http://www.dds.nl/~han/software/mg/mg-20090107.tar.gz
    File already downloaded and cached
    ==> ./configure
    ==> make prefix=/usr/local/Cellar/mg/20090107
    ==> make install prefix=/usr/local/Cellar/mg/20090107
    /usr/local/Cellar/mg/20090107: 3 files, 220K, built in 6 seconds

Success!  Now I can track my addition by committing the new formula
file to [my fork](http://github.com/seth/homebrew) of the homebrew
project.

