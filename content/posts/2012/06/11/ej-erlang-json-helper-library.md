--- 
title: Introducing ej, an Erlang JSON helper library
kind: article
created_at: Sat Jun 16 05:00:00 -0000 2012
tags:
- erlang
---

I wrote [ej][] to make it easier to work with JSON in Erlang. There are a
number of options available for parsing and encoding JSON in Erlang
such as [mochijson2][], [ejson][], and [jiffy][]. These libraries do a
good job of serialization/deserialization, but don't provide much in
the way of convenience when you need to manipulate the EJSON (Erlang
terms representing a JSON object) returned by the parsing
functions. And that's where ej comes in.

[mochijson2]: https://github.com/mochi/mochiweb
[ejson]: https://github.com/benoitc/ejson
[jiffy]: https://github.com/davisp/jiffy
[ej]: https://github.com/seth/ej/

## Getting and Setting with ej ##

You can use ej to extract values from EJSON, update values deep within
an EJSON structure, or validate that a parsed object matches a
specification that you provide.

Here's the minimal example for extracting and setting a value:

    % get a value
    %
    > ej:get({"hipster", "bike", "num_gears"}, Ejson).
    1

    % set a value
    %
    % building a ten-speed was never so easy
    > ej:set({"hipster", "bike", "num_gears"}, Ejson, 10).


## EJSON Validation ##

The ej module also provides a function (`ej:valid`) to validate an
EJSON object based on a pre-defined specification. The specification
describes required and optional keys as well as the types and formats
of their corresponding values within a JSON object.

If you are building a web service in Erlang that speaks JSON, you
might want something like `ej:valid` to validate the JSON data sent to
the server by clients. The function will return `ok` if the input
matches the spec and will otherwise return an `#ej_invalid{}` record
with details about what didn't match so that you can construct a
detailed error message.

Validation specs are described using the same shape of Erlang terms as
the desired EJSON, but with values replaced with _value specs_ that
determine what constitutes a valid value. This is probably best
explained with a corny example. Below is a spec for a "bike" object
and a "bike_store" object.

    bike_spec() ->
        {[
          {<<"brand">>, {string_match, regex_for(brand)}},
          {<<"price">>, number},
          {{opt, <<"color">>},
           {fun_match, {fun valid_color/1, string,
            <<"invalid color">>}}}
         ]}.
    
    bike_store_spec() ->
        {[
          {<<"name">>, string},
          {<<"bikes">>, {array_map, bike_spec()}}
         ]}.
    
    regex_for(brand) ->
        {ok, Regex} = re:compile("^[a-z]+$"),
        {Regex, <<"brand name can contain only letters a-z">>}.
    
    valid_color(C) when C =:= <<"red">>;         
                        C =:= <<"blue">>;
                        C =:= <<"silver">> ->
        ok;
    valid_color(_C) ->
        error.

And here are two example records to give you an idea of the
information contained when invalid EJSON is encountered:

    {[
      {<<"brand">>, <<"111 bad NAME">>},
      {<<"price">>, 600}
     ]}

    #ej_invalid{type = string_match,
                key = <<"brand">>,
                found = <<"111 bad NAME">>,
                found_type = string,
                expected_type = string,
                msg = <<"brand name can contain only letters a-z">>}


    {[
      {<<"brand">>, <<"bianci">>},
      {<<"price">>, [700]}
     ]}
     
     #ej_invalid{type = json_type,
                 key = <<"price">>,
                 found = [700],
                 found_type = array,
                 expected_type = number,
                 msg = undefined}

This functionality was motivated by a need we had at work to validate
complicated JSON structures in a web API we are writing in Erlang. The
version as it stands has proved quite useful, but I think there is
room for improvement in a few areas:

* Currently, additional keys present in the object are ignored. It
  would be good to optionally enforce a "just these keys and nothing
  more" semantic.

* Unify the structure of key and value specs; Allow a user-specified
  error message for all specs, but also allow specs to generate
  information. Consider a template-ish approach for specifying an
  error message with information that will be available.

* Fix the `object_map` spec to return information on key and value for
  non-matching data.

If you got this far and have an idea for how this validation tool
could be improved, please drop me a note.

## You can ej too ##

ej is on GitHub at
[https://github.com/seth/ej](https://github.com/seth/ej), the current
stable release tag is 0.0.2. You can read the [README][] file if you
like.

[README]: https://github.com/seth/ej/blob/master/README.org


