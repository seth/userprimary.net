This week I dove into Zope to help with the development of a webapp to manage genomic data.  Well, it was more of a belly flop than a dive.

There's a lot of existing code, an entire working prototype.  The main py file has 3487 lines!?

Trying to come up to speed so I could clean things up, I started reading about Zope webapp development.  I came across many documents claiming the superiority of ZPT over DTML.  You can preview your design in a browser, it is valid XML, and it is easier to achieve the nirvana of code/presentation separation.  Amen.

But then you go looking for examples.  Almost all of the Zope documentation has you creating everything via a web-interface (the ZMI).  I find this an excruciating way to develop.  Not to mention that it doesn't help us build a sharable, distributable "Product" and more than one person can work on (source code control?).

I managed to get basic ZPT stuff working from a file-based Product.  It was still painful and left me wondering why anyone would want to develop code with this stuff.  AFAICT, this is how you are supposed to return from a Python script and render a ZPT page "wsselectForm":

<pre>return getattr(self, "wsselectForm").__of__(self)(foo="bar", radios=radios)
</pre>

Barf.  I want no part of that.  Also, all of the examples use "context", but at the Product level, I've been unable to figure out how to access that or initialize it, or allow it.  I must be too dense.

Anyhow, I did get a number of the basics down and was able to significantly improve the error handling approach, but my feeling is to make this solid code it needs a rewrite and it isn't clear to me that Zope is where it should be rewritten.
