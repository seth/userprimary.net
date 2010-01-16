URL shortening services map long URLs to shorter URLs usually encoding the path with a sequence of letters and numbers (e.g. "/a2Xf3") and provide a redirect to the original URL when the short form is clicked. The shortened URLs are useful for Twitter tweets since they take up fewer of the 140 characters allotted to each tweet and can be easier to email since many email clients break long URLs into separate lines making them difficult to use (OS X's Mail.app is particularly bad as there is no way to get it not to corrupt your carefully formatted text/links/code by inserting line breaks willy nilly).

There are dozens of URL shortening services, although many appear to be little more than ad-traps.  The proliferation of these off-brand shortening services may be driven by spammers since the shortened URLs allow spammers to post links on sites that blacklist links to domains associated with spam.  The more reputable URL shortening services use blacklists like http://www.surbl.org/ to help prevent the use of their service for spam posting.  Some of the services also provide the ability to preview a shortened URL.  Not only can a preview help users decide whether or not they want to click through, but a site that receives posted links can use the preview in a programmatic fashion to remove short URLs that point to blacklisted domains.

I took a closer look at the APIs of those services that provide the ability to "preview" a shortened URL in addition to the standard redirection service.  Here are some notes.

<strong>ur.ly
</strong>
The site with the cleanest API is <a href="http://ur.ly/">ur.ly</a>, an open source URL shortening service hosted on Google's App Engine.  Here's an excerpt from the <a href="http://code.google.com/p/urly/wiki/APIDocumentation">ur.ly API docs</a>:

<quickcode:noclick>Create a New ur.ly

Create a new ur.ly and redirect to it (not useful, we know)
http://ur.ly/new?href={href}

Create a new ur.ly and show in HTML format
http://ur.ly/new.html?href={href}

Create a new ur.ly and show in JSON format
http://ur.ly/new.json?href={href}

Create a new ur.ly and show in XML format
http://ur.ly/new.xml?href={href}

Lookup an ur.ly

Redirect an ur.ly to its target HREF
http://ur.ly/{code}

Display an ur.ly in HTML format
http://ur.ly/{code}.html

Display an ur.ly in JSON format
http://ur.ly/{code}.json 

Display an ur.ly in XML format
http://ur.ly/{code}.xml
</quickcode>
I really like how simple they've made the API for shortening and previewing in the assortment of data formats.  Using the format extension on the shortened URL to get a preview makes sense to me and is easy to remember.

<strong>is.gd</strong>

The service at <a href="http://is.gd">is.gd</a> is also nice and simple, although they only provide HTML responses which makes the service far less useful for integrating into other applications.

<quickcode:noclick>Shorten
http://is.gd/api.php?longurl=http://www.example.com

Preview
http://is.gd/{code}-
</quickcode>So previewing with is.gd is very easy, just append a trailing '-' to the URL.  However, I find the trailing dash less intuitive then ur.ly's format extension.  Another feature of is.gd is that you can add additional path elements that are ignored for redirection.  So you can do http://is.gd/123/something.  I guess that's somewhat useful, although it obviously makes the URL longer...


<strong>bit.ly</strong>

The <a href="http://bit.ly">bit.ly</a> service has a number of additional features.  They have user accounts and provide the ability to track usage statistics of your shortened URLs.  Their API is <a href="http://code.google.com/p/bitly-api/wiki/ApiDocumentation">well documented</a> and reasonable, but much more complicated as a result of the user/developer accounts and additional features.  They host their API docs on Google code, which is an interesting approach for getting a cheap developer community setup.  Here's a summary of just the shorten and preview features:

<quickcode:noclick>/shorten
http://api.bit.ly/shorten?version=2.0.1&#38;longUrl=http://cnn.com&#38;login=bitlyapidemo&#38;apiKey=R_0da49e0a9118ff35f52f629d2d71bf07

/expand
http://api.bit.ly/expand?version=2.0.1&#38;shortUrl=http://bit.ly/31IqMl&#38;login=bitlyapidemo&#38;apiKey=R_0da49e0a9118ff35f52f629d2d71bf07
</quickcode>If bit.ly only provides shorten and preview, I would say that the /shorten and /expand resources are too verbose, but in the context of their API, it makes more sense since they also provide /info and /stats.  Like ur.ly, you can get responses in XML and JSON.  They also provide a JSONP callback mechanism.

<strong>snipurl.com</strong>

The winner of the make a simple API hard award goes to <a href="http://snipurl.com">snipurl</a> (aka snurl, snipr, sn.im).  Like bit.ly, snipurl offers user accounts and some additional statistics gathering for shortened URLs.  Their API is <a href="http://snurl.com/site/help?go=api">documented via PHP</a> code examples and requires an HTTP POST.

<blockquote>You can issue a POST request to the program: http://snipurl.com/site/getsnip -- the POST request should send the required information as indicated in blue highlight in the code below. Here is some sample code in PHP, for example, making use of the Curl library:

[followed by 49 lines of PHP code]</blockquote>Scrolling to the bottom of their docs, I noticed that they originally had a simpler API and that they moved to the new API as a result of abuse from unregistered users.  Given that their motivation was to make their API harder to use, I think they succeeded.

<strong>tr.im</strong>

To end on a positive note, I also found <a href="http://tr.im">tr.im</a> which not only has the best domain name of the bunch, but has a very clean API despite supporting a similar feature set as bit.ly in terms of accounts and statistics.  I'm not sure if it is just a matter of how the <a href="http://tr.im/api">API is documented</a>, but it feels a bit cleaner and easier than the bit.ly API.

<quickcode:noclick>Trim
http://tr.im/api/trim_url.json?url=http://www.google.com

Preview
http://tr.im/api/trim_destination.json?trimpath=abc
</quickcode>I especially like that their API can be used without an API key for low frequency use and testing, but that you can optionally obtain an API key for heavier use and to gain access to additional features.

<!-- technorati tags start --><p style="text-align:right;font-size:10px;">Technorati Tags: <a href="http://www.technorati.com/tag/programming" rel="tag">programming</a>, <a href="http://www.technorati.com/tag/REST" rel="tag">REST</a></p><!-- technorati tags end -->
