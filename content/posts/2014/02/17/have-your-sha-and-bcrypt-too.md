--- 
title: "Have Your SHA and Bcrypt Too"
kind: article
created_at: Mon Feb 17 11:08:04 -0800 2014
tags:
- programming
- security
---

## Fear

I've been putting off sharing this idea because I've
heard the rumors about what happens to folks who aren't security
experts when they post about security on the internet. If this blog is
replaced with cat photos and rainbows, you'll know what happened.

## The Sad Truth

It's 2014 and chances are you have accounts on websites that are not
properly handling user passwords. I did no research to produce the
following list of ways passwords are mishandled in decreasing order of
frequency:

1. Site uses a fast hashing algorithm, typically `SHA1(salt + plain-password)`.
2. Site doesn't salt password hashes
3. Site stores raw passwords

We know that sites should be generating secure random salts and using
an established slow hashing algorithm (bcrypt, scrypt, or PBKDF2). Why
are sites not doing this?

While security issues deserve a top spot on any site's priority list,
new features often trump addressing legacy security concerns. The
immediacy of the risk is hard to quantify and it's easy to fall prey
to a "nothing bad has happened yet, why should we change now"
attitude. It's easy for other bugs, features, or performance issues to
win out when measured by immediate impact. Fixing security or other
"legacy" issues is the Right Thing To Do and often you will see no
measurable benefit from the investment. It's like having
insurance. You don't need it until you do.

Specific to the improper storage of user password data is the issue of
the impact to a site imposed by upgrading. There are two common
approaches to upgrading password storage. You can switch cold turkey
to the improved algorithms and force password resets on all of your
users. Alternatively, you can migrate incrementally such that new
users and any user who changes their password gets the increased
security.

The cold turkey approach is not a great user experience and sites
might choose to delay an upgrade to avoid admitting to a weak
security implementation and disrupting their site by forcing password
resets.

The incremental approach is more appealing, but the security benefit
is drastically diminished for any site with a substantial set of
existing users.

Given the above migration choices, perhaps it's (slightly) less
surprising that businesses choose to prioritize other work ahead of
fixing poorly stored user password data.

## The Idea

What if you could upgrade a site so that both new and existing users
immediately benefited from the increased security, but without the
disruption of password resets? It turns out that you can and it isn't
very hard.

Consider a user table with columns:

    userid
    salt
    hashed_pass

Where the `hashed_pass` column is computed using a weak fast
algorithm, for example `SHA1(salt + plain_pass)`.

The core of the idea is to apply a proper algorithm on top of the data
we already have. I'll use `bcrypt` to make the discussion
concrete. Add columns to the user table as follows:

    userid
    salt
    hashed_pass
    hash_type
    salt2
    
Process the existing user table by computing `bcrypt(salt2 +
hashed_pass)` and storing the result in the `hashed_pass` column
(overwriting the less secure value); save the new salt value to
`salt2` and set `hash_type` to `bycrpt+sha1`.

To verify a user where `hash_type` is `bcrypt+sha1`, compute
`bcrypt(salt2 + SHA1(salt + plain_pass))` and compare to the
`hashed_pass` value. Note that bcrypt implementations encode the salt
as a prefix of the hashed value so you could avoid the `salt2` column,
but it makes the idea easier to explain to have it there.

You can take this approach further and have any user that logs in (as
well as new users) upgrade to a "clean" bcrypt only algorithm since
you can now support different verification algorithms using
`hash_type`. With the proper application code changes in place, the
upgrade can be done live.

This scheme will also work for sites storing non-salted password
hashes as well as those storing plain text passwords (THE HORROR).

## Less Sadness, Maybe

Perhaps this approach makes implementing a password storage security
upgrade more palatable and more likely to be prioritized. And if
there's a horrible flaw in this approach, maybe you'll let me know
without turning this blog into a tangle of cat photos and rainbows.
