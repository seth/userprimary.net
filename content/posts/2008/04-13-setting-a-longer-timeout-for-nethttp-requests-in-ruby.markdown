In order to exercise a RESTful web service I've been working on, I wrote a quick Ruby script to hammer the service with large update requests.  After uncovering and fixing a handful of concurrency issues in the server, I started seeing timeout errors in my test script when I sent numerous simultaneous updates.  The errors look like:

<pre>/opt/local/lib/ruby/1.8/timeout.rb:54:in `rbuf_fill': execution expired (Timeout::Error)
	from /opt/local/lib/ruby/1.8/timeout.rb:56:in `timeout'
	from /opt/local/lib/ruby/1.8/timeout.rb:76:in `timeout'
	from /opt/local/lib/ruby/1.8/net/protocol.rb:132:in `rbuf_fill'
	from /opt/local/lib/ruby/1.8/net/protocol.rb:116:in `readuntil'
	from /opt/local/lib/ruby/1.8/net/protocol.rb:126:in `readline'
	from /opt/local/lib/ruby/1.8/net/http.rb:2017:in `read_status_line'
	from /opt/local/lib/ruby/1.8/net/http.rb:2006:in `read_new'
	from /opt/local/lib/ruby/1.8/net/http.rb:1047:in `request'
	from /opt/local/lib/ruby/1.8/net/http.rb:1034:in `request'
	from /opt/local/lib/ruby/1.8/net/http.rb:543:in `start'
	from /opt/local/lib/ruby/1.8/net/http.rb:1032:in `request'
	from /opt/local/lib/ruby/1.8/net/http.rb:842:in `post'
	from ./rndsender.rb:21:in `post_update'
	from ./rndsender.rb:76:in `main'
	from ./rndsender.rb:80</pre>

Adding a rescue as shown below allows you to handle the timeout error:

<pre>def post_update(path, payload)
  http = Net::HTTP.new(@host, @port)
  res = http.post(path, payload, {'Content-Type' =>; 'application/xml'})
  case res
  when Net::HTTPSuccess
    puts "update posted"
  else
    res.error!
  end
  rescue Timeout::Error =>; e
    puts "update timeout error"
end
</pre>

After searching a bit on the web, I came across <a href="http://groups.google.com/group/rubyonrails-talk/browse_thread/thread/fdbc95dae6b7f5f2">this post</a> that had the magic incantation for adjusting the timeout in the Net::HTTP API.  Here it is:

<pre>http = Net::HTTP.new(@host, @port)
http.read_timeout = 500
</pre>

And in case you are interested in actually making use of the timeout, be warned!  <a href="http://headius.blogspot.com/2008/02/rubys-threadraise-threadkill-timeoutrb.html">Read this</a> in which you will learn that

<blockquote>Ruby's Thread#raise, Thread#kill, and the timeout.rb standard library based on them are inherently broken and should not be used for any purpose. And by extension, net/protocol.rb and all the net/* libraries that use timeout.rb are also currently broken (but they can be fixed).</blockquote>
<!-- technorati tags start --><p style="text-align:right;font-size:10px;">Technorati Tags: <a href="http://www.technorati.com/tag/programming" rel="tag">programming</a>, <a href="http://www.technorati.com/tag/Ruby" rel="tag">Ruby</a></p><!-- technorati tags end -->
