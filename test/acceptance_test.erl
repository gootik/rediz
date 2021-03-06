-module(acceptance_test).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

rediz_hash_test_() ->
    {foreach,
     fun() -> setup() end,
     fun(_) -> cleanup() end,
     hash_tests()}.

rediz_set_test_() ->
    {foreach,
     fun() -> setup() end,
     fun(_) -> cleanup() end,
     set_tests()}.

rediz_hll_test_() ->
    {foreach,
     fun() -> setup() end,
     fun(_) -> cleanup() end,
     hll_tests()}.

rediz_heavy_test_() ->
    {foreach,
     fun() -> setup() end,
     fun(_) -> cleanup() end,
     [fun test_hkeys_heavy/0]}.

rediz_init_test() ->
    rediz:start(rediz_test_pool, #{
        ip => "127.0.0.1",
        port => 6379,
        db => 5,
        auth => no_auth,
        pool_size => 1
    }),

    {ok, <<"OK">>} = rediz:set(<<"rediz:init_test">>, <<"db_5">>, rediz_test_pool),
    {ok, <<"db_5">>} = rediz:get(<<"rediz:init_test">>, rediz_test_pool),

    {ok, <<"OK">>} = rediz:query(<<"SELECT 1">>, rediz_test_pool),
    {ok, undefined} = rediz:get(<<"rediz:init_test">>, rediz_test_pool),

    cleanup().

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

hll_tests() ->
    [fun test_pfadd/0,
     fun test_pfadd_list/0,
     fun test_pfmerge/0,
     fun test_pfcount/0,
     fun test_pfcount_list/0].

set_tests() ->
    [fun test_sadd/0,
     fun test_scard/0,
     fun test_sdiff/0,
     fun test_sdiffstore/0,
     fun test_sinter/0,
     fun test_sinterstore/0,
     fun test_sismember/0,
     fun test_smembers/0,
     fun test_smove/0,
     fun test_spop/0,
     fun test_srandmember/0,
     fun test_srem/0,
     fun test_sunion/0,
     fun test_sunionstore/0].

%% Hash Tests
test_hlen() ->
    {ok, <<"OK">>} = rediz:query(<<"HMSET rediz:test:hash f1 v1 f2 v2 f3 v3">>, rediz_test_pool),
    {ok, 3} = rediz:hlen(<<"rediz:test:hash">>, rediz_test_pool).

test_hdel() ->
    {ok, <<"OK">>} = rediz:query(<<"HMSET rediz:test:hash f1 v1 f2 v2 f3 v3">>, rediz_test_pool),
    {ok, 1} = rediz:hdel(<<"rediz:test:hash">>, <<"f1">>, rediz_test_pool),
    {ok, 0} = rediz:hdel(<<"rediz:test:hash">>, <<"does_not_exist">>, rediz_test_pool),
    {ok, [{<<"f3">>, <<"v3">>},
          {<<"f2">>, <<"v2">>}]} = rediz:hgetall(<<"rediz:test:hash">>, rediz_test_pool).

test_hexists() ->
    {ok, <<"OK">>} = rediz:query(<<"HMSET rediz:test:hash f1 v1 f2 v2 f3 v3">>, rediz_test_pool),
    {ok, 1} = rediz:hexists(<<"rediz:test:hash">>, <<"f1">>, rediz_test_pool),
    {ok, 0} = rediz:hexists(<<"rediz:test:hash">>, <<"does_not_exist">>, rediz_test_pool).

test_hget() ->
    {ok, <<"OK">>} = rediz:query(<<"HMSET rediz:test:hash f1 v1 f2 v2 f3 v3">>, rediz_test_pool),
    {ok, <<"v1">>} = rediz:hget(<<"rediz:test:hash">>, <<"f1">>, rediz_test_pool).

test_hgetall() ->
    {ok, <<"OK">>} = rediz:query(<<"HMSET rediz:test:hash f1 v1 f2 v2 f3 v3">>, rediz_test_pool),
    {ok, [{<<"f3">>, <<"v3">>},
          {<<"f2">>, <<"v2">>},
          {<<"f1">>, <<"v1">>}]} = rediz:hgetall(<<"rediz:test:hash">>, rediz_test_pool).

test_hmget() ->
    {ok, <<"OK">>} = rediz:query(<<"HMSET rediz:test:hash f1 v1 f2 v2 f3 v3">>, rediz_test_pool),
    {ok, [<<"v2">>, <<"v1">>]} = rediz:hmget(<<"rediz:test:hash">>, [<<"f1">>, <<"f2">>], rediz_test_pool).

test_hset() ->
    {ok, 1} = rediz:hset(<<"rediz:test:hash">>, <<"hset_test">>, <<"done">>, rediz_test_pool),
    {ok, <<"done">>} = rediz:hget(<<"rediz:test:hash">>, <<"hset_test">>, rediz_test_pool).

test_hsetnx() ->
    {ok, 1} = rediz:hsetnx(<<"rediz:test:hash">>, <<"hset_test">>, <<"done">>, rediz_test_pool),
    {ok, <<"done">>} = rediz:hget(<<"rediz:test:hash">>, <<"hset_test">>, rediz_test_pool),
    {ok, 0} = rediz:hsetnx(<<"rediz:test:hash">>, <<"hset_test">>, <<"done">>, rediz_test_pool).

test_hmset() ->
    {ok, <<"OK">>} = rediz:hmset(<<"rediz:test:hash">>, [{<<"f1">>, <<"v1">>},
                                                         {<<"f2">>, <<"v2">>},
                                                         {<<"f3">>, <<"v3">>}], rediz_test_pool),
    {ok, [{<<"f3">>, <<"v3">>},
          {<<"f2">>, <<"v2">>},
          {<<"f1">>, <<"v1">>}]} = rediz:hgetall(<<"rediz:test:hash">>, rediz_test_pool).

test_hkeys() ->
    {ok, <<"OK">>} = rediz:query(<<"HMSET rediz:test:hash f1 v1 f2 v2 f3 v3">>, rediz_test_pool),
    {ok, [<<"f3">>,
          <<"f2">>,
          <<"f1">>]} = rediz:hkeys(<<"rediz:test:hash">>, rediz_test_pool).

test_hvals() ->
    {ok, <<"OK">>} = rediz:query(<<"HMSET rediz:test:hash f1 v1 f2 v2 f3 v3">>, rediz_test_pool),
    {ok, [<<"v3">>,
          <<"v2">>,
          <<"v1">>]} = rediz:hvals(<<"rediz:test:hash">>, rediz_test_pool).

test_hstrlen() ->
    {ok, 1} = rediz:query(<<"HSET rediz:test:hash f1 1234">>, rediz_test_pool),
    {ok, 4} = rediz:hstrlen(<<"rediz:test:hash">>, <<"f1">>, rediz_test_pool).

test_sadd() ->
    {ok, 1} = rediz:sadd(<<"rediz:test:set">>, <<"sadd_test">>, rediz_test_pool),
    {ok, 0} = rediz:sadd(<<"rediz:test:set">>, <<"sadd_test">>, rediz_test_pool),

    {ok, [<<"sadd_test">>]} = rediz:smembers(<<"rediz:test:set">>, rediz_test_pool).

test_scard() ->
    {ok, 1} = rediz:sadd(<<"rediz:test:set">>, <<"sadd_test">>, rediz_test_pool),
    {ok, 1} = rediz:sadd(<<"rediz:test:set">>, <<"sadd_test2">>, rediz_test_pool),
    {ok, 2} = rediz:scard(<<"rediz:test:set">>, rediz_test_pool).

test_sdiff() ->
    {ok, 1} = rediz:sadd(<<"rediz:test:set">>, <<"a">>, rediz_test_pool),
    {ok, 1} = rediz:sadd(<<"rediz:test:set">>, <<"b">>, rediz_test_pool),
    {ok, 1} = rediz:sadd(<<"rediz:test:set">>, <<"c">>, rediz_test_pool),
    {ok, 1} = rediz:sadd(<<"rediz:test:set">>, <<"d">>, rediz_test_pool),

    {ok, 1} = rediz:sadd(<<"rediz:test:set2">>, <<"c">>, rediz_test_pool),

    {ok, 1} = rediz:sadd(<<"rediz:test:set3">>, <<"a">>, rediz_test_pool),
    {ok, 1} = rediz:sadd(<<"rediz:test:set3">>, <<"c">>, rediz_test_pool),
    {ok, 1} = rediz:sadd(<<"rediz:test:set3">>, <<"e">>, rediz_test_pool),

    {ok, R1} = rediz:sdiff(<<"rediz:test:set">>, [<<"rediz:test:set2">>, <<"rediz:test:set3">>], rediz_test_pool),
    [<<"b">>, <<"d">>] = lists:sort(R1).

test_sdiffstore() ->
    {ok, 1} = rediz:sadd(<<"rediz:test:set">>, <<"a">>, rediz_test_pool),
    {ok, 1} = rediz:sadd(<<"rediz:test:set">>, <<"b">>, rediz_test_pool),
    {ok, 1} = rediz:sadd(<<"rediz:test:set">>, <<"c">>, rediz_test_pool),
    {ok, 1} = rediz:sadd(<<"rediz:test:set">>, <<"d">>, rediz_test_pool),

    {ok, 1} = rediz:sadd(<<"rediz:test:set2">>, <<"c">>, rediz_test_pool),

    {ok, 1} = rediz:sadd(<<"rediz:test:set3">>, <<"a">>, rediz_test_pool),
    {ok, 1} = rediz:sadd(<<"rediz:test:set3">>, <<"c">>, rediz_test_pool),
    {ok, 1} = rediz:sadd(<<"rediz:test:set3">>, <<"e">>, rediz_test_pool),

    {ok, 2} = rediz:sdiffstore(<<"rediz:test:set_store">>, <<"rediz:test:set">>, [<<"rediz:test:set2">>, <<"rediz:test:set3">>], rediz_test_pool).

test_sinter() ->
    {ok, 1} = rediz:sadd(<<"rediz:test:set">>, <<"a">>, rediz_test_pool),
    {ok, 1} = rediz:sadd(<<"rediz:test:set">>, <<"b">>, rediz_test_pool),
    {ok, 1} = rediz:sadd(<<"rediz:test:set">>, <<"c">>, rediz_test_pool),

    {ok, 1} = rediz:sadd(<<"rediz:test:set2">>, <<"c">>, rediz_test_pool),
    {ok, 1} = rediz:sadd(<<"rediz:test:set2">>, <<"d">>, rediz_test_pool),
    {ok, 1} = rediz:sadd(<<"rediz:test:set2">>, <<"e">>, rediz_test_pool),

    {ok, [<<"c">>]} = rediz:sinter(<<"rediz:test:set">>, [<<"rediz:test:set2">>], rediz_test_pool).

test_sinterstore() ->
    {ok, 1} = rediz:sadd(<<"rediz:test:set">>, <<"a">>, rediz_test_pool),
    {ok, 1} = rediz:sadd(<<"rediz:test:set">>, <<"b">>, rediz_test_pool),
    {ok, 1} = rediz:sadd(<<"rediz:test:set">>, <<"c">>, rediz_test_pool),

    {ok, 1} = rediz:sadd(<<"rediz:test:set2">>, <<"c">>, rediz_test_pool),
    {ok, 1} = rediz:sadd(<<"rediz:test:set2">>, <<"d">>, rediz_test_pool),
    {ok, 1} = rediz:sadd(<<"rediz:test:set2">>, <<"e">>, rediz_test_pool),

    {ok, 1} = rediz:sinterstore(<<"rediz:test:set_store">>, <<"rediz:test:set">>, [<<"rediz:test:set2">>], rediz_test_pool).

test_sismember() ->
    {ok, 1} = rediz:sadd(<<"rediz:test:set">>, <<"sadd_test">>, rediz_test_pool),

    {ok, 1} = rediz:sismember(<<"rediz:test:set">>, <<"sadd_test">>, rediz_test_pool),
    {ok, 0} = rediz:sismember(<<"rediz:test:set">>, <<"no_member">>, rediz_test_pool).

test_smembers() ->
    {ok, 1} = rediz:sadd(<<"rediz:test:set">>, <<"a">>, rediz_test_pool),
    {ok, 1} = rediz:sadd(<<"rediz:test:set">>, <<"b">>, rediz_test_pool),

    {ok, R1} = rediz:smembers(<<"rediz:test:set">>, rediz_test_pool),
    [<<"a">>, <<"b">>] = lists:sort(R1).

test_smove() ->
    {ok, 1} = rediz:sadd(<<"rediz:test:set">>, <<"a">>, rediz_test_pool),

    {ok, 1} = rediz:sadd(<<"rediz:test:set2">>, <<"b">>, rediz_test_pool),
    {ok, 1} = rediz:sadd(<<"rediz:test:set2">>, <<"c">>, rediz_test_pool),

    {ok, 1} = rediz:smove(<<"rediz:test:set2">>, <<"rediz:test:set">>, <<"b">>, rediz_test_pool),

    {ok, R1} = rediz:smembers(<<"rediz:test:set">>, rediz_test_pool),
    {ok, [<<"c">>]} = rediz:smembers(<<"rediz:test:set2">>, rediz_test_pool),

    [<<"a">>, <<"b">>] = lists:sort(R1).

test_spop() ->
    {ok, 1} = rediz:sadd(<<"rediz:test:set">>, <<"a">>, rediz_test_pool),

    {ok, <<"a">>} = rediz:spop(<<"rediz:test:set">>, rediz_test_pool).

test_srandmember() ->
    {ok, 1} = rediz:sadd(<<"rediz:test:set">>, <<"b">>, rediz_test_pool),

    {ok, [<<"b">>]} = rediz:srandmember(<<"rediz:test:set">>, rediz_test_pool).

test_srem() ->
    {ok, 1} = rediz:sadd(<<"rediz:test:set">>, <<"b">>, rediz_test_pool),

    {ok, 1} = rediz:srem(<<"rediz:test:set">>, <<"b">>, rediz_test_pool),
    {ok, 0} = rediz:srem(<<"rediz:test:set">>, <<"b">>, rediz_test_pool).

test_sunion() ->
    {ok, 1} = rediz:sadd(<<"rediz:test:set">>, <<"a">>, rediz_test_pool),
    {ok, 1} = rediz:sadd(<<"rediz:test:set">>, <<"b">>, rediz_test_pool),
    {ok, 1} = rediz:sadd(<<"rediz:test:set">>, <<"c">>, rediz_test_pool),

    {ok, 1} = rediz:sadd(<<"rediz:test:set2">>, <<"c">>, rediz_test_pool),
    {ok, 1} = rediz:sadd(<<"rediz:test:set2">>, <<"d">>, rediz_test_pool),
    {ok, 1} = rediz:sadd(<<"rediz:test:set2">>, <<"e">>, rediz_test_pool),

    Expected = lists:sort([<<"d">>,
                           <<"a">>,
                           <<"b">>,
                           <<"e">>,
                           <<"c">>]),
    {ok, Result} = rediz:sunion([<<"rediz:test:set">>, <<"rediz:test:set2">>], rediz_test_pool),

    Expected = lists:sort(Result).


test_sunionstore() ->
    {ok, 1} = rediz:sadd(<<"rediz:test:set">>, <<"a">>, rediz_test_pool),
    {ok, 1} = rediz:sadd(<<"rediz:test:set">>, <<"b">>, rediz_test_pool),
    {ok, 1} = rediz:sadd(<<"rediz:test:set">>, <<"c">>, rediz_test_pool),

    {ok, 1} = rediz:sadd(<<"rediz:test:set2">>, <<"c">>, rediz_test_pool),
    {ok, 1} = rediz:sadd(<<"rediz:test:set2">>, <<"d">>, rediz_test_pool),
    {ok, 1} = rediz:sadd(<<"rediz:test:set2">>, <<"e">>, rediz_test_pool),

    Expected = lists:sort([<<"d">>,
                           <<"a">>,
                           <<"b">>,
                           <<"e">>,
                           <<"c">>]),
    {ok, 5} = rediz:sunionstore(<<"rediz:test:store">>, [<<"rediz:test:set">>, <<"rediz:test:set2">>], rediz_test_pool),
    {ok, Result} = rediz:smembers(<<"rediz:test:store">>, rediz_test_pool),

    Expected = lists:sort(Result).

%% HyperLogLog Tests
test_pfadd() ->
    {ok, 1} = rediz:pfadd(<<"rediz:test:hll">>, <<"id1">>, rediz_test_pool),
    {ok, 0} = rediz:pfadd(<<"rediz:test:hll">>, <<"id1">>, rediz_test_pool).

test_pfadd_list() ->
    {ok, 1} = rediz:pfadd(<<"rediz:test:hll">>, [<<"id1">>, <<"id2">>], rediz_test_pool),
    {ok, 0} = rediz:pfadd(<<"rediz:test:hll">>, [<<"id1">>, <<"id2">>], rediz_test_pool),
    {ok, 1} = rediz:pfadd(<<"rediz:test:hll">>, [<<"id1">>, <<"id2">>, <<"id3">>], rediz_test_pool).

test_pfmerge() ->
    {ok, 1} = rediz:pfadd(<<"rediz:test:hll1">>, [<<"id1">>, <<"id2">>], rediz_test_pool),
    {ok, 1} = rediz:pfadd(<<"rediz:test:hll2">>, [<<"id1">>, <<"id2">>, <<"id3">>], rediz_test_pool),
    {ok, <<"OK">>} = rediz:pfmerge(<<"rediz:test:hll3">>, [<<"rediz:test:hll1">>, <<"rediz:test:hll2">>], rediz_test_pool),
    {ok, 3} = rediz:pfcount(<<"rediz:test:hll3">>, rediz_test_pool).

test_pfcount() ->
    {ok, 1} = rediz:pfadd(<<"rediz:test:hll">>, [<<"id1">>, <<"id2">>], rediz_test_pool),
    {ok, 2} = rediz:pfcount(<<"rediz:test:hll">>, rediz_test_pool),

    {ok, 0} = rediz:pfadd(<<"rediz:test:hll">>, [<<"id1">>, <<"id2">>], rediz_test_pool),
    {ok, 2} = rediz:pfcount(<<"rediz:test:hll">>, rediz_test_pool),

    {ok, 1} = rediz:pfadd(<<"rediz:test:hll">>, [<<"id1">>, <<"id3">>], rediz_test_pool),
    {ok, 3} = rediz:pfcount(<<"rediz:test:hll">>, rediz_test_pool).

test_pfcount_list() ->
    {ok, 1} = rediz:pfadd(<<"rediz:test:hll1">>, [<<"id1">>, <<"id2">>], rediz_test_pool),
    {ok, 1} = rediz:pfadd(<<"rediz:test:hll2">>, [<<"id1">>, <<"id2">>, <<"id3">>], rediz_test_pool),
    {ok, 3} = rediz:pfcount([<<"rediz:test:hll1">>, <<"rediz:test:hll2">>], rediz_test_pool).


test_hkeys_heavy() ->
    lists:foreach(
        fun(_) ->
            RandomField = list_to_binary(random_string(8)),
            {ok, 1} = rediz:hset(<<"rediz:test:hash_keys">>, RandomField, <<"1">>, rediz_test_pool)
        end, lists:seq(1, 10000)),

    {ok, Keys} = rediz:hkeys(<<"rediz:test:hash_keys">>, rediz_test_pool),
    10000 = length(Keys).

setup() ->
    rediz:start(rediz_test_pool, #{
        ip => "127.0.0.1",
        port => 6379,
        db => 1,
        auth => no_auth
    }).

cleanup() ->
    rediz:query(<<"FLUSHALL">>, rediz_test_pool),
    ok = shackle_pool:stop(rediz_test_pool).

random_string(N) ->
    Alpha = "1234567890-=abcdefghijklmnopqrstuvwxyz,./;[",
    L = length(Alpha),
    lists:map(
        fun(_) ->
            I = granderl:uniform(L),
            lists:nth(I, Alpha)
        end, lists:seq(1, N)).