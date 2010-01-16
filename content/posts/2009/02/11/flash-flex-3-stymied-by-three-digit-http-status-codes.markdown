Adobe's <a href="http://www.adobe.com/products/flex/">Flex 3</a>, the language and runtime behind Flash applications on the web, has <a href="http://bugs.adobe.com/jira/browse/SDK-11841">a long standing bug</a> of not being able to process HTTP requests when the server responds with a non-200 response code.

This would be a rather serious limitation if the Flex 3 technology was intended for consuming multiple web services to bring together data available on the web in the context of an engaging and interactive user experience.  Oh, wait.  That <em>is</em> what Flex is designed for and this <em>is</em> a serious limitation.  Flex 3 gives you the ability to do HTT, but seems to leave out the "P" for Protocol; rather <em>in</em>flexible, if you ask me.

And yet Flash's popularity for creating interactive page elements and Adobe's failure to fix bugs drives silliness like the following in the <a href="http://apiwiki.twitter.com/REST+API+Documentation#Parameters">Twitter REST API</a>:

<blockquote>suppress_response_codes: If this parameter is present, all responses will be returned with a 200 OK status code - even errors.  This parameter exists to accommodate Flash and JavaScript applications running in browsers that intercept all non-200 responses.  If used, it's then the job of the client to determine error states by parsing the response body.  Use with caution, as those error messages may change.</blockquote>
It is frustrating to see a popular, but broken, client force extra work on the server side.  But that's <a href="http://www.microsoft.com/windows/downloads/ie/getitnow.mspx">nothing new</a>.

The response from Adobe does not leave one particularly optimistic for a quick fix (or even any fix).  Here's one of the last comments on <a href="http://bugs.adobe.com/jira/browse/SDK-11841">WebService fault details are hidden by Flex SDK</a>, a bug filed in Adobe's public bug repository describing the issue:

<blockquote>Peter Farland - [03/03/08 02:18 PM ]
Please read Matt's comment above yours... unfortunately the Flex team cannot fix this issue until the Flash Player team fixes their issue, which in turn relies on cooperation from various browsers.</blockquote>
Hopefully there are obscure technical reasons why it is difficult for the flash players on Firefox and Safari to pass HTTP status codes around.  But you have to admit it is pretty surprising.  Concerned by the comments about limitations in Firefox I verified that you can indeed access HTTP status codes and response bodies <em>even when the response code is not 200</em>.  Here's an example that worked for me using JQuery:

<pre lang="javascript">
        $.ajax({
            url: '/the500',
            type: 'GET',
            dataType: 'json',
            timeout: 1000,
            error: function(event, request, settings) {
                // here's the HTTP status and message from the HTTP header
                var msg = event.status + " " + event.statusText;
                // and here's the body text
                msg += " " + event.responseText.substring(0, 100);
                $('#status').text(msg);
            },
            success: function(json){
                // do something with the data.
            }
        });
</pre>
<!-- technorati tags start --><p style="text-align:right;font-size:10px;">Technorati Tags: <a href="http://www.technorati.com/tag/programming" rel="tag">programming</a>, <a href="http://www.technorati.com/tag/REST" rel="tag">REST</a></p><!-- technorati tags end -->
