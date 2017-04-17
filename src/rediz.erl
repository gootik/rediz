-module(rediz).

-include("rediz.hrl").

-export([
    start/2,

    %% Key Commands
    set/3, get/2, keys/2, del/2,

    %% Hash Commands
    hlen/2, hdel/3, hexists/3,
    hget/3, hgetall/2, hmget/3,
    hincrby/4, hincrbyfloat/4,
    hset/4, hsetnx/4, hmset/3,
    hkeys/2, hvals/2, hstrlen/3,

    %% HyperLogLog Commands
    pfadd/3, pfcount/2, pfmerge/3,

    %% Sets
    sadd/3, scard/2,
    sdiff/2, sdiff/3, sdiffstore/3, sdiffstore/4,
    sinter/2, sinter/3, sinterstore/3, sinterstore/4,
    sismember/3, smembers/2,
    smove/4, spop/2, spop/3,
    srandmember/2, srandmember/3,
    srem/3, sunion/2, sunionstore/3,

    %% Scans
    %% sscan, hscan, scan

    %% Query
    query/2
]).

-spec start(atom, map()) -> {ok, atom()}.
start(Name, Options) ->
    rediz_app:start(),
    ok = rediz_sup:start_pool(Name, Options),

    {ok, Name}.

%% Key Commands
-spec set(binary(), binary(), atom()) -> rediz_reply().
set(Key, Value, Pool) ->
    call_rediz({set, Key, Value}, Pool).

-spec get(binary(), atom()) -> rediz_reply().
get(Key, Pool) ->
    call_rediz({get, Key}, Pool).

-spec del(binary(), atom()) -> rediz_reply().
del(Key, Pool) ->
    call_rediz({del, Key}, Pool).

-spec keys(binary(), atom()) -> rediz_reply().
keys(Pattern, Pool) ->
    call_rediz({keys, Pattern}, Pool).

%% Hash Commands
-spec hdel(binary(), binary() | [binary()], atom()) -> rediz_reply().
hdel(Key, Fields, Pool) when is_list(Fields) ->
    call_rediz({hdel, Key, Fields}, Pool);
hdel(Key, Field, Pool) ->
    hdel(Key, [Field], Pool).

-spec hexists(binary(), binary(), atom()) -> rediz_reply().
hexists(Key, Field, Pool) ->
    call_rediz({hexists, Key, Field}, Pool).

-spec hincrby(binary(), binary(), integer(), atom()) -> rediz_reply().
hincrby(Key, Field, Val, Pool) when is_integer(Val) ->
    call_rediz({hincrby, Key, Field, Val}, Pool).

-spec hincrbyfloat(binary(), binary(), float(), atom()) -> rediz_reply().
hincrbyfloat(Key, Field, Val, Pool) when is_float(Val) ->
    call_rediz({hincrbyfloat, Key, Field, Val}, Pool).

-spec hkeys(binary(), atom()) -> rediz_reply().
hkeys(Key, Pool) ->
    call_rediz({hkeys, Key}, Pool).

-spec hlen(binary(), atom()) -> rediz_reply().
hlen(Key, Pool) ->
    call_rediz({hlen, Key}, Pool).

-spec hmget(binary(), [binary()], atom()) -> rediz_reply().
hmget(Key, Fields, Pool) when is_list(Fields) ->
    call_rediz({hmget, Key, Fields}, Pool).

-spec hmset(binary(), [{binary(), binary()}], atom()) -> rediz_reply().
hmset(Key, FieldValues, Pool) when is_list(FieldValues) ->
    call_rediz({hmset, Key, FieldValues}, Pool).

-spec hget(binary(), binary(), atom()) -> rediz_reply().
hget(Key, Field, Pool) ->
    call_rediz({hget, Key, Field}, Pool).

-spec hset(binary(), binary(), binary(), atom()) -> rediz_reply().
hset(Key, Field, Value, Pool) ->
    call_rediz({hset, Key, Field, Value}, Pool).

-spec hsetnx(binary(), binary(), binary(), atom()) -> rediz_reply().
hsetnx(Key, Field, Value, Pool) ->
    call_rediz({hsetnx, Key, Field, Value}, Pool).

-spec hstrlen(binary(), binary(), atom()) -> rediz_reply().
hstrlen(Key, Field, Pool) ->
    call_rediz({hstrlen, Key, Field}, Pool).

-spec hgetall(binary(), atom()) -> {ok, [tuple()]}.
hgetall(Key, Pool) ->
    {ok, ValList} = call_rediz({hgetall, Key}, Pool),

    {ok, redis_array_to_proplist(ValList)}.

-spec hvals(binary(), atom()) -> rediz_reply().
hvals(Key, Pool) ->
    call_rediz({hvals, Key}, Pool).

%% HyperLogLog Commands
-spec pfadd(binary(), binary() | [binary()], atom()) -> rediz_reply().
pfadd(Key, Values, Pool) when is_list(Values) ->
    call_rediz({pfadd, Key, Values}, Pool);
pfadd(Key, Value, Pool) ->
    pfadd(Key, [Value], Pool).

-spec pfcount(binary() | [binary()], atom()) -> rediz_reply().
pfcount(Keys, Pool) when is_list(Keys) ->
    call_rediz({pfcount, Keys}, Pool);
pfcount(Key, Pool) ->
    pfcount([Key], Pool).

-spec pfmerge(binary(), binary() | [binary()], atom()) -> rediz_reply().
pfmerge(DestKey, SourceKeys, Pool) when is_list(SourceKeys) ->
    call_rediz({pfmerge, DestKey, SourceKeys}, Pool);
pfmerge(DestKey, SourceKey, Pool) ->
    pfmerge(DestKey, [SourceKey], Pool).


%% Sets
-spec sadd(binary(), binary() | [binary()], atom()) -> rediz_reply().
sadd(Key, Members, Pool) when is_list(Members) ->
    call_rediz({sadd, Key, Members}, Pool);
sadd(Key, Member, Pool) ->
    sadd(Key, [Member], Pool).

-spec scard(binary(), atom()) -> rediz_reply().
scard(Key, Pool) ->
    call_rediz({scard, Key}, Pool).

-spec sdiff(binary(), [binary()], atom()) -> rediz_reply().
sdiff(Key, Keys, Pool) when is_list(Keys) ->
    call_rediz({sdiff, Key, Keys}, Pool).

-spec sdiff(binary(), atom()) -> rediz_reply().
sdiff(Key, Pool) ->
    sdiff(Key, [], Pool).

-spec sdiffstore(binary(), binary(), [binary()], atom()) -> rediz_reply().
sdiffstore(Destination, Key, Keys, Pool) when is_list(Keys) ->
    call_rediz({sdiffstore, Destination, Key, Keys}, Pool).

-spec sdiffstore(binary(), binary(), atom()) -> rediz_reply().
sdiffstore(Destination, Key, Pool) ->
    sdiffstore(Destination, Key, [], Pool).

-spec sinter(binary(), [binary()], atom()) -> rediz_reply().
sinter(Key, Keys, Pool) when is_list(Keys) ->
    call_rediz({sinter, Key, Keys}, Pool).

-spec sinter(binary(), atom()) -> rediz_reply().
sinter(Key, Pool) ->
    sinter(Key, [], Pool).

-spec sinterstore(binary(), binary(), [binary()], atom()) -> rediz_reply().
sinterstore(Destination, Key, Keys, Pool) when is_list(Keys) ->
    call_rediz({sinterstore, Destination, Key, Keys}, Pool).

-spec sinterstore(binary(), binary(), atom()) -> rediz_reply().
sinterstore(Destination, Key, Pool) ->
    sinterstore(Destination, Key, [], Pool).

-spec sismember(binary(), binary(), atom()) -> rediz_reply().
sismember(Key, Member, Pool) ->
    call_rediz({sismember, Key, Member}, Pool).

-spec smembers(binary(), atom()) -> rediz_reply().
smembers(Key, Pool) ->
    call_rediz({smembers, Key}, Pool).

-spec smove(binary(), binary(), binary(), atom()) -> rediz_reply().
smove(Source, Destination, Member, Pool) ->
    call_rediz({smove, Source, Destination, Member}, Pool).

-spec spop(binary(), non_neg_integer(), atom()) -> rediz_reply().
spop(Key, Count, Pool) ->
    call_rediz({spop, Key, Count}, Pool).

-spec spop(binary(), atom()) -> rediz_reply().
spop(Key, Pool) ->
    spop(Key, 1, Pool).

-spec srandmember(binary(), non_neg_integer(), atom()) -> rediz_reply().
srandmember(Key, Count, Pool) ->
    call_rediz({srandmember, Key, Count}, Pool).

-spec srandmember(binary(), atom()) -> rediz_reply().
srandmember(Key, Pool) ->
    spop(Key, 1, Pool).

-spec srem(binary(), binary() | [binary()], atom()) -> rediz_reply().
srem(Key, Members, Pool) when is_list(Members) ->
    call_rediz({srem, Key, Members}, Pool);
srem(Key, Member, Pool) ->
    srem(Key, [Member], Pool).

-spec sunion(binary() | [binary()], atom()) -> rediz_reply().
sunion(Keys, Pool) when is_list(Keys) ->
    call_rediz({sunion, Keys}, Pool);
sunion(Key, Pool) ->
    sunion([Key], Pool).

-spec sunionstore(binary(), binary() | [binary()], atom()) -> rediz_reply().
sunionstore(Destination, Keys, Pool) when is_list(Keys) ->
    call_rediz({sunionstore, Destination, Keys}, Pool);
sunionstore(Destination, Key, Pool) ->
    sunionstore(Destination, [Key], Pool).

-spec query(binary(), atom()) -> rediz_reply().
query(Query, Pool) when is_binary(Query) ->
    call_rediz({raw_query, Query}, Pool).

call_rediz(Query, Pool) ->
    shackle:call(Pool, Query).

redis_array_to_proplist([]) ->
    [];
redis_array_to_proplist([V, K | Rest]) ->
    [{K, V} | redis_array_to_proplist(Rest)].