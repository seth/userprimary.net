--- 
title: Optimizing Nanoc Based Websites for Fun and Profit
kind: article
created_at: Mon Jan 10 21:00:00 -0800 2011
tags:
- nanoc
- webdev
---

One of the tasks that recently came my way at work was to improve the
performance of a new [nanoc][]-based website we will be rolling out
soon.  I went to [YSlow][] and [Page Speed][] (especially
[this][intro]) for advice and came up with some nice optimizations
that can be plugged into any nanoc-based site.  So I thought I'd
share.

[intro]: http://code.google.com/speed/page-speed/docs/rules_intro.html
[nanoc]: http://nanoc.stoneship.org/
[YSlow]: http://developer.yahoo.com/yslow/
[Page Speed]: http://code.google.com/speed/page-speed/

## Combining multiple CSS/Javascript files into a single asset ##

As with many sites, there are a handful of separate CSS and Javascript
files used in our site's design.  Keeping the files separate is
helpful for keeping the source of the site organized and easy to
extend.  However, there is no reason to pay for multiple HTTP requests
when serving the site.  Combing all of the CSS for a page into a
single asset and creating a similar combined asset for Javascript will
reduce the number of HTTP requests required to load the page,
decreasing load time.

The recipe for doing this in nanoc is:

1. Create a new item to represent the combined asset.  Give this item
   a `combined` attribute, and use the template system of your choice
   to concatenate the parts into a single file.

2. Setup `compile` and `route` rules to make a special case for the
   items with attribute `combined`.

Here's an example of a combined css `Nanoc3::Item`.  I've used haml, as that's
what we are using on our site, but erb would work just as well:

    ---
    combined: true
    ---
    - base_css_items.each do |i|
      = i.compiled_content

In the example, I used a helper function, `base_css_items`, that
returns a list of `Nanoc3::Item`s that represent the CSS files that
should be combined.

For the `Rules` file, here are the relevant bits:

    compile '/css/*/' do
      if item[:combined]
        filter :haml
      end
    end

    route '/css/*/' do
      if item[:combined]
        item.identifier.chop + '.css'
      else
        nil
      end
    end

So when compiling the css files, only those items with a `combined`
attribute are filtered through haml, the rest are not touched.  For
routing, only the combined file gets routed to an output file.

A similar treatment will get you combined Javascript files.

## Compressing (minifying) CSS and Javascript ##

Now that you have reduced the number of HTTP requests required to load
your CSS and Javascript, you might want to minify these files to
reduce their size and (slightly) increase the speed at which they can
be processed by browsers.  To do this, I turned to the [YUI
Compressor][] and wrote a [nanoc filter][1] that can be used to
compress CSS or Javascript.  If you put the YUI Compressor filter in
`lib/default.rb`, you can minify the combined CSS from the example
above like this:

    compile '/css/*/' do
      if item[:combined]
        filter :haml
        filter :yui_compress
      end
    end

Note that the filter assumes that you have Java available on your PATH
and that the YUI Compressor jar file is in a top-level `tools`
directory for your site.

Because the filter invokes Java, it can feel a bit slow.  But if
you've combined your CSS and Javascript, you'll likely only have to
run it over a small number of files.

[1]: https://gist.github.com/774015 
[YUI Compressor]: http://developer.yahoo.com/yui/compressor/

## Fingerprint filenames for expires header happiness ##

One of the ways to improve page performance, especially for repeat
customers, is to make good use of browser caching.  The Page Speed
docs have a nice [overview][ps_lbc] of the why and how.  The short
version is:

* Use the `expires` HTTP header and set a value far into the future
  (e.g. one year).

* For content files that you might want to change like CSS and
  Javascript, add a fingerprint to content filenames that is based on
  the file's contents (e.g. md5).  This way, if you change the
  content, the filename will change, and browsers will download the
  new file.

[ps_lbc]: http://code.google.com/speed/page-speed/docs/caching.html#LeverageBrowserCaching

We can have nanoc help us achieve this using a few helper functions.
First, we need a helper that computes the MD5 digest of a list of
items:

    require 'digest/md5'

    def digest_for_items(items)
      digest = Digest::MD5.new
      items.each { |x| digest << x.raw_content }
      digest.hexdigest
    end

To continue with our combined CSS file example, we'll add a helper
that calls `digest_for_items` on the `Nanoc3::Item`s that make up our
combined CSS file and returns the fingerprinted path for the combined
file based on its contents:

    def base_css_digest
      digest = digest_for_items(base_css_items)
      "/css/#{digest}-base.css"
    end

The `base_css_digest` can be used in content files that use the CSS.
The final step is to modify the routing rule so that the combined file
gets written out to its fingerprinted name:

    route '/css/*/' do
      case item.identifier
      when '/css/combined-base/'
        base_css_digest
      else
        nil
      end
    end

To have all of this hard work pay off, you will need to modify your
web server to set the expires header for the content that you've
fingerprinted.  Here's an example of what I did to get the expires
headers to be generated using nginx:

    location /css {
      expires 350d;
    }

Happy caching.

## Automatically include height and width for all <img> tags ##

One last optimization. Specifying the dimensions of images in `<img>`
tags can speed up browser rendering (see [here][imgs] for details).
Here's a helper function that uses the [image_size][] gem to compute a
given image's dimensions and generate an appropriate `<img>` tag.

    require 'image_size'

    def image_with_size(path, css_class="")
      img = ImageSize.new(IO.read("content/#{path}"))
      klass = if css_class.empty?
                ""
              else
                'class="%s" ' % css_class
              end
      fmt = '<img src="%s" height="%d" width="%d" %s/>'
      fmt % [path, img.height, img.width, klass]
    end


[imgs]: http://code.google.com/speed/page-speed/docs/rendering.html#SpecifyImageDimensions
[image_size]: http://rubygems.org/gems/image_size/versions/1.0.1
