I spent this week exploring <a href="http://couchdb.apache.org/">CouchDB</a> as a backing store for a data aggregation service that we are building at work.  The service needs to analyze web server access logs and provide aggregated views of the data over different time windows.  For the purposes of this discussion, the analysis goal is to identify the most viewed pages.  As each page has associated with it a list of tags, we also wish to rank the tags based on the page views.

There are many capable packages that will parse, analyze, and report on web server access logs and we are certainly not trying to reinvent the wheel.  However, the specifics of our data and they way in which we wish to serve the results make the various off-the-shelf analysis packages inappropriate.  

CouchDB is compelling because:

<ul>
	<li>the document-based paradigm coupled with the map/reduce powered views makes experimenting with the data cheap and gives one hope that the system will support as yet to be defined requirements;</li>
<li>we can process data and then serve it up via HTTP for other services to consume using the same system in which the data is stored;</li>
<li>couchdb has a trigger-based replication mechanism that could be used in setting up a highly available service</li>
<li>If you take all the pillows off, you can make an awesome fort</li>
</ul>

After reading through <a href="http://wiki.apache.org/couchdb/How_to_handle_stats_aggregation">How to handle stats aggregation</a> on the CouchDB wiki, I decided to process the raw log data into one minute summaries rather than creating a document in couchdb for every log entry.  The first step was to gather some archived access log data and split it into files representing one minute worth of requests.  Then I wrote a script to summarize each one minute chunk into a list of unique pages and counts that could be inserted as documents into couchdb.  In the production system, the idea is that the live access logs would be summarized into one minute chunks and integrated into the database in near real-time.   Here's an example document:

<pre lang="javascript">
{
       "_id" : "2009-06-13T11h07_70af27180205e9dd37322fdaa92dd60e",
   "request" : "/the/url/for/this/request";
"view_count" : 120;
  "view_pct" : 0.15;
      "tags" : ["foo", "bar", "baz"];
}
</pre>

I used the <a href="http://github.com/jchris/couchrest/tree/master">couchrest</a> Ruby gem to insert the documents into couchdb.  Next it was time to start writing some couchdb views to start looking at the data.  I did a lot of experimenting using temporary views entered via the futon web interface.  This eased the learning curve considerably because I was able to get rapid feedback on my first attempts to write couchdb map/reduce views.

We want to be able to see the most viewed pages for a specified one minute, one hour, one day, or one month interval.  Ideally, I would like to have a view that allowed for more flexibility in the aggregation of time, for example, a view that would allow a user to specify a ten minute or four hour interval; I haven't found a good solution for that yet.

But if you can live with predefined time intervals, one approach that seems to work is to define a map function that emits a key for each time interval and page ID.  In the example below, the document IDs consist of a timestamp, coded like 2009-06-13T11h07, a separating underscore, and then the md5 digest of the page's URL.

<pre lang="javascript">
// map function for view count by minute, hour, day, or month
function(doc) {
    var doc_hash = doc._id.replace(/^.+_/, "");
    var date_str = doc._id.replace(/_.+$/, "").replace(/\-/g, "/").
        replace("T", " ").replace("h", ":") + " PDT";
    var dt = new Date(Date.parse(date_str));
    var date_key = [dt.getFullYear(), dt.getMonth() + 1,
                    dt.getDate(), dt.getHours(), dt.getMinutes()];
    var date_keys = {
        "M" : date_key.join("-"),
        "H" : date_key.slice(0, 4).join("-"),
        "D" : date_key.slice(0,3).join("-"),
        "m" : date_key.slice(0,2).join("-")
    };
    for (t in date_keys) {
        emit([t, date_keys[t], doc_hash], doc.view_count);
    }
}

// reduce function
function(keys, values, rere) {
    return sum(values);
}
</pre>

Given a document with ID "2009-06-13T10h31_abc", the map function will emit keys:
<pre lang="javascript">
["M", "2009-6-13-10-31", "abc"]
["H", "2009-6-13-10", "abc"]
["D", "2009-6-13", "abc"]
["m", "2009-6", "abc"]
</pre>

One can then query this view for a specific time interval using the group, startkey, and endkey parameters.  For example, to get a summary of page views on June 12, 2009 between 10:00 and 11:00, issue a query with startkey=["H", "2009-6-12-10", true], endkey=["H", "2009-6-12-10", {}] and group=true.  Based on my experiments and reading of the view collation rules, this will return all of the hour-based keys that look like "2009-6-12-10" (the hour between 10 and 11).  The reduce function and group=true will sum the view counts for each unique page ID.

What this view does not do is return the results sorted by page views.  The lack of an ability to sort results based on a computed value was surprising to me and has me reconsidering whether or not couchdb is appropriate for our needs.  For now we can get around this by ask for all results for the view and doing the sorting on the client side.  Another missing piece is that CouchDB currently lacks a notion of chaining map/reduce views together.  If we could run one more map on the result of this view, we could easily sort by the view_count.  The Erlang-based client <a href="http://github.com/jchris/hovercraft/commits/master">hovercraft</a> already has support for view chaining by inserting the results of a view into a temporary database.  There has also been some <a href="http://mail-archives.apache.org/mod_mbox/couchdb-dev/200906.mbox/%3c1D7D2ECC-A0F7-4E04-B1FA-299132A1B1B3@geni.com%3e">recent discussion on the couchdb developer mailing list</a> about possible approaches to support view chaining in general.

Creating a view for the tags follows similarly by emitting a row for each timestamp and tag combination.  Another approach would be to create a separate view for each time unit instead of relying on the array-valued keys and prefixes.  Some experiments are in order to understand further the pros and cons of a single view vs seprate views.  One could even take the one view to rule them all approach further and add the tag rows to the same view by prepending another element into the key array to identify the tag rows.

Overall, I've been impressed with CouchDB's ease of use and the performance appears to be quite usable for our purposes, although I haven't yet done any formal measurements.  I do have some reservations, however, that our use cases differ significantly from those that drive couchdb development.

