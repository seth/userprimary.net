I had a chance to play with <a href="http://couchdb.org/CouchDB/CouchDBWeb.nsf/Home?OpenForm">CouchDB</a> last week and carry out a few performance related experiments.  The installation and initial configuration went surprisingly smoothly considering that CouchDB is a new project using a language, <a href="http://www.erlang.org/">Erlang</a>, that is not yet standard fare.  I used the apt-get targets suggested on the CouchDB wiki, ran configure and make, and issued a few chmod commands so that my test user could access the default locations for CouchDB's data, log and run directories and was up and running within a half hour.

The next pleasant surprise was the web-based database browser that comes with the installation.  Not only did it provide immediate feedback that I had a working installation, but the GUI actually allows you to do useful things like create/delete database and add/edit/remove documents.  Here's a screenshot showing the document view for one of my test documents:

<img src="http://userprimary.net/user/wp-content/uploads/2007/12/200712151644.jpg" height="300" width="464" border="1" hspace="4" vspace="4" alt="200712151644" />

Next, I installed the <a href="http://couchobject.rubyforge.org/">CouchObject</a> Ruby gem to quickly have a way of working with CouchDB from Ruby.  The hardest part here was figuring out that you have to <span style="font-family:monospace;font-size:9pt;">require 'rubygems'</span> to use gems.  If Ruby can find <span style="font-family:monospace;font-size:9pt;">rubygems</span> after I install it and if I call <span style="font-family:monospace;font-size:9pt;">gem install</span> as root, why can't Ruby find those packages without additional help?  Sigh.  Anyhow, once I could load the gem, using it to manipulate CouchDB was no problem.

<strong>Performance Experiments
</strong>
First of all, as is clear from the above, this was my first time using CouchDB and it is entirely possible that I missed some configuration levers that would totally change the numbers I saw.  Second, whether the numbers below look good or bad to you will depend on the end use you have in mind -- perhaps CouchDB isn't the right hammer for me.

The test machine is a dual Xeon 2.4MHz server with 4GB RAM running Debian 4.0.  The system has one ~60GB SCSI disk.  All tests were run with CouchDB 0.7.2.

All tests use a small document template that looks like this:

<pre>
// Example document
{
"name":12345, // integer
"color":"white",
"type":"washer",
"tstamp":12345 // long
} </pre>

For all of the following tests, the client was run on the same server that was running CouchDB.

<strong>Test 1</strong>: Sequential Document Creation

For Test 1, I created new documents in an empty database one after another in a loop.

<pre>
=== Add single doc in a loop ===
|      N |  sec | Docs/sec |
|--------+------+----------|
|   1000 |    9 |      111 |
|  10000 |  102 |       98 |
| 100000 | 1075 |       93 |
</pre>

The times scale linearly, but I was surprised to see a 1.3GB file size for the database in the 100K case.  Given the small example document, I estimated an optimistic lower bound on the space required for storage:

<pre>
"name"   => 4B
"color"  => 9B (5B for "white", 4B int for length)
"type"   => 10B
"tstamp" => 8B
"_id"    => 36B (assume internally assigned ID stored
                 as 32B string + length)
</pre>


The total is 67B/doc, but let's call it 100B/doc to have a nice round number.  So 100K documents translates to 9MB.  A final DB size of 1.3GB implies an inflation factor of 148.  I know disk space is cheap, but is it that cheap?


<strong>Test 2</strong>: Bulk Document Creation

For Test 2, I used the _bulk_docs API to create each set of documents in one call.

<pre>
=== Add N docs in bulk ===
|      N | sec | Docs/sec |
|--------+-----+----------|
|   1000 | 0.6 | 1667     |
|  10000 |  15 | 667      |
| 100000 |  NA | NA       |
</pre>

When attempting to add 100K documents in bulk the client said:
<pre>
    /usr/lib/ruby/1.8/timeout.rb:54:in `rbuf_fill': execution expired (Timeout::Error)
</pre>

And the server said:
<pre>
    eheap_alloc: Cannot allocate 729810240 bytes of memory (of type "heap").
    Aborted
</pre>

Here I was surprised both that 100K documents was enough to exhaust the system's memory and that the result was a complete crash of the server.  I wonder how hard it would be to take advantage of Erlang's touted fault tolerant features to make the server more robust to such situations.

<strong>Test 3</strong>: Repeated Bulk Document Creation

In Test 3, I ran a loop in which I added 1000 documents via the bulk API in loop.  I was able to add 200K documents in 96 seconds.  The big surprise was that this resulted in a DB size of 38MB (an inflate factor of 2).  It is not clear to me why the DB size varies so much between the document at a time and bulk access methods.  But it does suggest that if you know how to use CouchDB, you can get very good performance and that, as CouchDB developer Damien Katz puts forth in <a href="http://damienkatz.net/2007/12/thoughts_on_opt.html">this post on his blog</a>, there is ample room for some big optimizations in CouchDB.

<strong>Conclusions</strong>

<ol>
<li>CouchDB was easy to install and use.  The available documentation was helpful.</li>
<li>Performance-wise, it is fairly easy to discover limitations of the current system (as is completely reasonable, IMO, for an alpha release) especially for use cases involving lots of documents.</li>
<li>My take away is that CouchDB is a compelling solution for many use cases, but not yet ready for large collections with high throughput demands.  I suspect that is going to change as performance optimizations are implemented and replication and partitioning features are added in the future.</li>
</ol>



<!-- technorati tags start --><p style="text-align:right;font-size:10px;">Technorati Tags: <a href="http://www.technorati.com/tag/databases" rel="tag">databases</a>, <a href="http://www.technorati.com/tag/programming" rel="tag">programming</a>, <a href="http://www.technorati.com/tag/scalability" rel="tag">scalability</a></p><!-- technorati tags end -->
