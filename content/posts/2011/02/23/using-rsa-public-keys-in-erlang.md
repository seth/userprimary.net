--- 
title: Using RSA Public Keys in Erlang
kind: article
created_at: Wed Feb 23 19:54:31 -0800 2011
tags:
- erlang
- crypto
---

## Or How I Earned a Merit Badge by Contributing a Patch to OTP ##

This all started with an effort to make it easier to build
Erlang-based services at [work][].  I decided to port Opscode's
[mixlib-authentication][] library to Erlang (see [chef_authn][], if
you're curious) and ran into some limitations of Erlang's `public_key`
module for dealing with PEM encoded RSA public keys.  While sorting
out a workaround, I ended up polishing [a patch] which will be
included in the next Erlang release.

[work]: http://opscode.com/
[mixlib-authentication]: https://github.com/opscode/mixlib-authentication
[chef_authn]: https://github.com/seth/chef_authn
[a patch]: https://github.com/erlang/otp/commit/c730d2fb0342523fa9014373b234b426bd9ca6f2

Clients of the Chef server use mixlib-authentication to sign
requests.  This involves encrypting a message digest of the request
path and body along with other request details using the client's
private key and packaging the authentication data as a collection
base-64 encoded headers.  The Chef server then uses
mixlib-authentication to decrypt the authentication data using the
public key stored for the client and verifying the request.

In order to authenticate a request, one needs to be able to decrypt a
message using a public key.  Chef uses RSA keys for authentication and
the Chef server stores the public keys for clients in PEM format as is
output by the `openssl` command line tool.

It turns out that in Erlang 14B (`public_key` 0.8), there is no easy
to way make use of an RSA public key in the format typically available
from openssl.  This was surprising to me because of openssl's ubiquity
and the name of the module being "public_key".

What I ended up learning is that the PEM files one gets from openssl
are in *subject public key info* format, a standard format that
contains both key data and an algorithm identifier.  Incidentally,
this explains why both RSA and DSA keys in this format can begin with a
generic header line of `-----BEGIN PUBLIC KEY-----` and be used
without knowing ahead of time which algorithm is intended.  Armed with
this info and some hints generously provided by the erlang-questions
list, I was able work around the `public_key` module's limitation as
follows:

    read_rsa_public_key(Key) ->
        Bin = erlang:iolist_to_binary(public_key_lines(re:split(Key, "\n"), [])),
        Spki = public_key:der_decode('SubjectPublicKeyInfo', base64:mime_decode(Bin)),
        {_, _, {0, KeyDer}} = Spki,
        public_key:der_decode('RSAPublicKey', KeyDer).
    
    public_key_lines([<<"-----BEGIN PUBLIC KEY-----">>|Rest], Acc) ->
        public_key_lines(Rest, Acc);
    public_key_lines([<<"-----END PUBLIC KEY-----">>|_Rest], Acc) ->
        lists:reverse(Acc);
    public_key_lines([Line|Rest], Acc) ->
        public_key_lines(Rest, [Line|Acc]).

Seeing an opportunity to improve the `public_key` module and, to be
honest, hoping to get a patch into the Erlang sources, I cooked up
some changes that add more straight-forward support for subject public
key info encoded keys.  Special thanks to Ingela Andin on the OTP team
at Ericson for answering questions, providing some critical hints, and
shepherding the patch through the review process.  Here's how you can
read RSA public keys in the up and coming version of the `public_key`
module:

    {ok, RSAPubPem} = file:read_file("rsa_pub.pem"),
    PemEntries = public_key:pem_decode(RSAPubPem),
    RSAPubKey = public_key:pem_entry_decode(hd(PemEntries)),

at which point happiness ensues and I get my badge (one of many, but
the first one for Erlang).

<a href="http://www.nerdmeritbadges.com/products/octocat">
<img src="/uploads/2011/02/open_source_contrib_badge.png" />
</a>
