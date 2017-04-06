-module(acceptance_test).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

rediz_hash_test_() ->
    {setup,
     fun() -> rediz:start() end,
     fun(_) -> cleanup() end,
     hash_tests()}.


hash_tests() ->
    [fun test_hlen/0,
     fun test_hdel/0,
     fun test_hexists/0,
     fun test_hget/0,
     fun test_hgetall/0,
     fun test_hmget/0].


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
    {ok, [{<<"f1">>, <<"v1">>},
          {<<"f3">>, <<"v3">>},
          {<<"f2">>, <<"v2">>}]} = rediz:hgetall(<<"rediz:test:hash">>).

test_hmget() ->
    {ok, <<"OK">>} = rediz:query(<<"HMSET rediz:test:hash f1 v1 f2 v2 f3 v3">>),
    {ok, [<<"v2">>, <<"v1">>]} = rediz:hmget(<<"rediz:test:hash">>, [<<"f1">>, <<"f2">>]).

cleanup() ->
    rediz:query(<<"FLUSHDB">>),
    rediz_app:stop(normal).
