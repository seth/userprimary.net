--- 
title: Generating XML in Erlang Using xmerl
kind: article
created_at: Wed Feb 16 21:54:17 -0800 2011
tags:
- erlang
- xml
---

Here's a quick example of how to build XML documents in Erlang using
xmerl.  The basic idea is to first transform your data into xmerl's
"simple" format of tagged tuples and then use `xmerl:export_simple` to
generate the XML.

The xmerl module's simple format maps an XML node to a tuple like so:

    {TAG_NAME, TUPLE_LIST_OF_ATTRIBUTES, CONTENT_LIST}
    % also
    {TAG_NAME, CONTENT_LIST}

where `TAG_NAME` is an atom, `TUPLE_LIST_OF_ATTRIBUTES` is what it
sounds like, and `CONTENT_LIST` contains items that are either
themselves nodes in xmerl format, bare tags (atoms), or *stringish
items* consisting of strings, binaries, and deep lists of same.

To demonstrate how one might go about coding up a function to
transform some data into XML, I'll use the XML format used by Solr for
document indexing.  Basically, the goal is to take a list of fields
(name/value pairs) like this:

     [{"id", "9885A004"},
      {"name", "Canon PowerShot SD500"},
      {"category", "camera"},
      {"features", "3x optical zoom"},
      {"features", "aluminum case"},
      {"weight", "6.4"},
      {"price", "329.95"}
     ]

and transform them into XML like this:

    Example from http://www.xml.com/lpt/a/1668
    <add>
      <doc>
        <field name="id">9885A004</field>
        <field name="name">Canon PowerShot SD500</field>
        <field name="category">camera</field>
        <field name="features">3x optical zoom</field>
        <field name="features">aluminum case</field>
        <field name="weight">6.4</field>
        <field name="price">329.95</field>
      </doc>
    </add>

First, a function to convert a tuple list to `field` nodes in xmerl
format:

    fields_to_xml_simple(Fields) ->
        [ {field, [{name, K}], [V]} || {K, V} <- Fields ].

Next a function to generate the XML wrapper nodes, again in xmerl
format:

    doc_xml_simple(Fields) ->
        {add, [{doc, fields_to_xml_simple(Fields)}]}.

Finally, a function to transform xmerl's format into XML:

    -define(xml_prolog, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>").

    make_doc_xml(Fields) ->
        Xml = xmerl:export_simple([doc_xml_simple(Fields)], xmerl_xml,
                                  [{prolog, ?xml_prolog}]),
        unicode:characters_to_binary(Xml).

And that's it.
