# Rediz
**NOTE**: Somewhat production ready....

[![Build Status](https://travis-ci.org/shezarkhani/rediz.svg?branch=master)](https://travis-ci.org/shezarkhani/rediz)

An Erlang Redis client based on the High-performance Shackle client library.
A lot of this library's design was inspired and driven by the design/code in [Anchor](https://github.com/lpgauth/anchor), 
and [Shackle](https://github.com/lpgauth/shackle).

## TODO
* Configuration for startup
* Being able to run multiple redis connection pools
* Rest of client commands
* Faster decoding
* Multi/Exec commands
* Redis AUTH on connection
* Run SELECT DB command on connection

## Why
To get familiar with Shackle by doing a simple/small project.

## Example
```
1> rediz:start().
{ok, [granderl, shackle, rediz]}

2> rediz:hset(<<"rediz:hash">>, <<"field">>, <<"val">>).
{ok, 1}

3> rediz:hset(<<"rediz:hash">>, <<"field2">>, <<"val2">>).
{ok, 1}

4> rediz:hkeys(<<"redis:hash">>).
{ok, []}

5> rediz:hkeys(<<"rediz:hash">>).
{ok, [<<"field2">>, <<"field">>]}

6> rediz:hget(<<"rediz:hash">>, <<"field2">>).
{ok, <<"val2">>}

7> rediz:hgetall(<<"rediz:hash">>).
{ok,[{<<"field2">>, <<"val2">>},
     {<<"field">>, <<"val">>}]}

8> rediz:keys(<<"rediz:*">>).
{ok, [<<"rediz:hash">>]}
```