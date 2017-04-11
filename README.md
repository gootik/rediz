# Rediz
**NOTE**: Somewhat production ready.... (Some big features are not implemented, but if this subset satisfies your needs then it is wokring well...)

[![Build Status](https://travis-ci.org/shezarkhani/rediz.svg?branch=master)](https://travis-ci.org/shezarkhani/rediz)

An Erlang Redis client based on the High-performance Shackle client library.
A lot of this library's design was inspired and driven by the design/code in [Anchor](https://github.com/lpgauth/anchor), 
and [Shackle](https://github.com/lpgauth/shackle).

## TODO
* Rest of client commands
* Faster decoding
* Multi/Exec commands

## Why
To get familiar with Shackle by doing a simple/small project.

## Example
```
1> rediz:start(rediz_local_pool, #{ip => "127.0.0.1", port => 6379, auth => no_auth, db => 0}).
ok

2> rediz:hset(<<"rediz:hash">>, <<"field">>, <<"val">>, rediz_local_pool).
{ok, 1}

3> rediz:hset(<<"rediz:hash">>, <<"field2">>, <<"val2">>, rediz_local_pool).
{ok, 1}

4> rediz:hkeys(<<"redis:hash">>, rediz_local_pool).
{ok, []}

5> rediz:hkeys(<<"rediz:hash">>, rediz_local_pool).
{ok, [<<"field2">>, <<"field">>]}

6> rediz:hget(<<"rediz:hash">>, <<"field2">>, rediz_local_pool).
{ok, <<"val2">>}

7> rediz:hgetall(<<"rediz:hash">>, rediz_local_pool).
{ok,[{<<"field2">>, <<"val2">>},
     {<<"field">>, <<"val">>}]}

8> rediz:keys(<<"rediz:*">>, rediz_local_pool).
{ok, [<<"rediz:hash">>]}
```
