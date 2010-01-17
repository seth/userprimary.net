I noticed that zLOG is now deprecated and the deprecation message said to use standard Python logging.

One way I found to get this working was to add the following to my module .py file in the product:

<pre>import logging
LOG = logging.getLogger("mymodule")

</pre>Then you can use LOG.error("foo") to send log messages.
To see the "mymodule" identifier in the logs you may want to modify your zope.conf file like so:



&lt;eventlog&gt;
  level debug
  &lt;logfile&gt;
    path $INSTANCE/log/event.log
    level debug
    format %(asctime)s %(name)-12s %(levelname)-8s %(message)s
  &lt;/logfile&gt;
&lt;/eventlog&gt;


Now I can see log messages and don't get a deprecation warning about it.  Yay.  
