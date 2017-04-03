-module(rediz).

-include("rediz.hrl").

-export([
    start/0,

    %% Key Commands
    set/2, get/1, keys/1, del/1,

    %% Hash Commands
    hlen/1, hdel/2, hexists/2,
    hget/2, hgetall/1, hmget/2,
    hincrby/3, hincrbyfloat/3,
    hset/3, hsetnx/3, hmset/2,
    hkeys/1, hvals/1, hstrlen/2,

    %% HyperLogLog Commands
    pfadd/2, pfcount/1, pfmerge/2,

    %% Query
    query/1
]).

-spec start() -> {ok, [atom()]} | {error, term()}.
start() ->
    rediz_app:start().

%% Key Commands
-spec set(binary(), binary()) -> rediz_reply().
set(Key, Value) ->
    call_rediz({set, Key, Value}).

-spec get(binary()) -> rediz_reply().
get(Key) ->
    call_rediz({get, Key}).

-spec del(binary()) -> rediz_reply().
del(Key) ->
    call_rediz({del, Key}).

-spec keys(binary()) -> rediz_reply().
keys(Pattern) ->
    call_rediz({keys, Pattern}).

%% Hash Commands
-spec hdel(binary(), binary()) -> rediz_reply().
hdel(Key, Field) ->
    call_rediz({hdel, Key, Field}).

-spec hexists(binary(), binary()) -> rediz_reply().
hexists(Key, Field) ->
    call_rediz({hexists, Key, Field}).

-spec hincrby(binary(), binary(), integer()) -> rediz_reply().
hincrby(Key, Field, Val) when is_integer(Val) ->
    call_rediz({hincrby, Key, Field, Val}).

-spec hincrbyfloat(binary(), binary(), float()) -> rediz_reply().
hincrbyfloat(Key, Field, Val) when is_float(Val) ->
    call_rediz({hincrbyfloat, Key, Field, Val}).

-spec hkeys(binary()) -> rediz_reply().
hkeys(Key) ->
    call_rediz({hkeys, Key}).

-spec hlen(binary()) -> rediz_reply().
hlen(Key) ->
    call_rediz({hlen, Key}).

-spec hmget(binary(), binary()) -> rediz_reply().
hmget(Key, Fields) ->
    call_rediz({hmget, Key, Fields}).

-spec hmset(binary(), #{binary() => binary()}) -> rediz_reply().
hmset(Key, FieldValues) when is_map(FieldValues) ->
    call_rediz({hmset, Key, FieldValues}).

-spec hget(binary(), binary()) -> rediz_reply().
hget(Key, Field) ->
    call_rediz({hget, Key, Field}).

-spec hset(binary(), binary(), binary()) -> rediz_reply().
hset(Key, Field, Value) ->
    call_rediz({hset, Key, Field, Value}).

-spec hsetnx(binary(), binary(), binary()) -> rediz_reply().
hsetnx(Key, Field, Value) ->
    call_rediz({hsetnx, Key, Field, Value}).

-spec hstrlen(binary(), binary()) -> rediz_reply().
hstrlen(Key, Field) ->
    call_rediz({hstrlen, Key, Field}).

-spec hgetall(binary()) -> rediz_reply().
hgetall(Key) ->
    call_rediz({hgetall, Key}).

-spec hvals(binary()) -> rediz_reply().
hvals(Key) ->
    call_rediz({hvals, Key}).

%% HyperLogLog Commands
-spec pfadd(binary(), binary() | [binary()]) -> rediz_reply().
pfadd(Key, Values) when is_list(Values) ->
    call_rediz({pfadd, Key, Values});
pfadd(Key, Value) ->
    pfadd(Key, [Value]).

-spec pfcount(binary() | [binary()]) -> rediz_reply().
pfcount(Keys) when is_list(Keys) ->
    call_rediz({pfcount, Keys});
pfcount(Key) ->
    pfcount([Key]).

-spec pfmerge(binary(), binary() | [binary()]) -> rediz_reply().
pfmerge(DestKey, SourceKeys) when is_list(SourceKeys) ->
    call_rediz({pfmerge, DestKey, SourceKeys});
pfmerge(DestKey, SourceKey) ->
    pfmerge(DestKey, [SourceKey]).

-spec query(binary()) -> rediz_reply().
query(Query) when is_binary(Query) ->
    call_rediz({raw_query, Query}).

call_rediz(Query) ->
    shackle:call(rediz, Query).