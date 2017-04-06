-module(acceptance_test).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

rediz_hash_test_() ->
    {foreach,
     fun() -> rediz:start() end,
     fun(_) -> cleanup() end,
     hash_tests()}.


hash_tests() ->
    [fun test_hlen/0,
     fun test_hdel/0,
     fun test_hexists/0,
     fun test_hget/0,
     fun test_hgetall/0,
     fun test_hmget/0,
     fun test_hset/0,
     fun test_hsetnx/0,
     fun test_hmset/0,
     fun test_hkeys/0,
     fun test_hvals/0].

test_hlen() ->
    {ok, <<"OK">>} = rediz:query(<<"HMSET rediz:test:hash f1 v1 f2 v2 f3 v3">>),
    {ok, 3} = rediz:hlen(<<"rediz:test:hash">>).

test_hdel() ->
    {ok, <<"OK">>} = rediz:query(<<"HMSET rediz:test:hash f1 v1 f2 v2 f3 v3">>),
    {ok, 1} = rediz:hdel(<<"rediz:test:hash">>, <<"f1">>),
    {ok, 0} = rediz:hdel(<<"rediz:test:hash">>, <<"does_not_exist">>),
    {ok, [{<<"f3">>, <<"v3">>},
          {<<"f2">>, <<"v2">>}]} = rediz:hgetall(<<"rediz:test:hash">>).

test_hexists() ->
    {ok, <<"OK">>} = rediz:query(<<"HMSET rediz:test:hash f1 v1 f2 v2 f3 v3">>),
    {ok, 1} = rediz:hexists(<<"rediz:test:hash">>, <<"f1">>),
    {ok, 0} = rediz:hexists(<<"rediz:test:hash">>, <<"does_not_exist">>).

test_hget() ->
    {ok, <<"OK">>} = rediz:query(<<"HMSET rediz:test:hash f1 v1 f2 v2 f3 v3">>),
    {ok, <<"v1">>} = rediz:hget(<<"rediz:test:hash">>, <<"f1">>).

test_hgetall() ->
    {ok, <<"OK">>} = rediz:query(<<"HMSET rediz:test:hash f1 v1 f2 v2 f3 v3">>),
    {ok, [{<<"f3">>, <<"v3">>},
          {<<"f2">>, <<"v2">>},
          {<<"f1">>, <<"v1">>}]} = rediz:hgetall(<<"rediz:test:hash">>).

test_hmget() ->
    {ok, <<"OK">>} = rediz:query(<<"HMSET rediz:test:hash f1 v1 f2 v2 f3 v3">>),
    {ok, [<<"v2">>, <<"v1">>]} = rediz:hmget(<<"rediz:test:hash">>, [<<"f1">>, <<"f2">>]).

test_hset() ->
    {ok, 1} = rediz:hset(<<"rediz:test:hash">>, <<"hset_test">>, <<"done">>),
    {ok, <<"done">>} = rediz:hget(<<"rediz:test:hash">>, <<"hset_test">>).

test_hsetnx() ->
    {ok, 1} = rediz:hsetnx(<<"rediz:test:hash">>, <<"hset_test">>, <<"done">>),
    {ok, <<"done">>} = rediz:hget(<<"rediz:test:hash">>, <<"hset_test">>),
    {ok, 0} = rediz:hsetnx(<<"rediz:test:hash">>, <<"hset_test">>, <<"done">>).

test_hmset() ->
    {ok, <<"OK">>} = rediz:hmset(<<"rediz:test:hash">>, [{<<"f1">>, <<"v1">>},
                                                         {<<"f2">>, <<"v2">>},
                                                         {<<"f3">>, <<"v3">>}]),
    {ok, [{<<"f3">>, <<"v3">>},
          {<<"f2">>, <<"v2">>},
          {<<"f1">>, <<"v1">>}]} = rediz:hgetall(<<"rediz:test:hash">>).

test_hkeys() ->
    {ok, <<"OK">>} = rediz:query(<<"HMSET rediz:test:hash f1 v1 f2 v2 f3 v3">>),
    {ok, [<<"f3">>,
          <<"f2">>,
          <<"f1">>]} = rediz:hkeys(<<"rediz:test:hash">>).

test_hvals() ->
    {ok, <<"OK">>} = rediz:query(<<"HMSET rediz:test:hash f1 v1 f2 v2 f3 v3">>),
    {ok, [<<"v3">>,
          <<"v2">>,
          <<"v1">>]} = rediz:hvals(<<"rediz:test:hash">>).

test_hstrlen() ->
    {ok, 1} = rediz:query(<<"HSET rediz:test:hash f1 1234">>),
    {ok, 4} = rediz:hstrlen(<<"rediz:test:hash">>, <<"f1">>).

cleanup() ->
    rediz:query(<<"FLUSHDB">>),
    rediz_app:stop(normal).
