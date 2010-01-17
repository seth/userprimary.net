The word <em>transparent</em> is often used in descriptions of programs and their API's.  Here's one example <a href="http://java.sun.com/docs/books/tutorial/security1.2/summary/apicore.html#KeyInts">from some Java documentation</a>:

<blockquote>KeyFactory

The KeyFactory class is an engine class designed to provide conversions between opaque cryptographic keys (of type Key) and key specifications (transparent representations of the underlying key material).</blockquote>Quite reasonable.  Opaque and transparent are evocative and describe key features of the objects that are players in the API.  But I often see transparent used when, IMO, opaque makes much more sense.  Consider <a href="http://www.xfront.com/REST-Web-Services.html">this page</a> describing REST webservices.

<blockquote>The web service makes available a URL to a parts list resource. For example, a client would use this URL to get the parts list:

    http://www.parts-depot.com/parts

Note that "how" the web service generates the parts list is completely transparent to the client. All the client knows is that if he/she submits the above URL then a document containing the list of parts is returned. Since the implementation is transparent to clients, Parts Depot is free to modify the underlying implementation of this resource without impacting clients. This is loose coupling.</blockquote>The use of transparent means to me that the user <em>would</em> understand how the services generates the parts list.  I think the word opaque fits much better.  Am I missing something?  When describing parts of a system that are decoupled from other parts, the details of the implementation should be opaque to other components, not transparent.  Thank you.
