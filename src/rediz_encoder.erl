%%%-------------------------------------------------------------------
%%% @doc rediz_encoder will encode a client message to a RESP command
%%%      to be sent to the server.
%%%
%%%      Please look at https://redis.io/topics/protocol for specifics
%%%      on the protocol.
%%% @end
%%%-------------------------------------------------------------------
-module(rediz_encoder).

-include("rediz.hrl").

-export([
    resp_encode/1
]).

-compile(inline).

-spec resp_encode(rediz_command()) -> binary().
%% Key Commands
resp_encode({set, Key, Value}) ->
    <<"SET ", Key/binary, " ", Value/binary>>;
resp_encode({get, Key}) ->
    <<"GET ", Key/binary>>;
resp_encode({keys, Pattern}) ->
    <<"KEYS ", Pattern/binary>>;

%% Hash Commands
resp_encode({hlen, Key}) ->
    <<"HLEN ", Key/binary>>;
resp_encode({hdel, Key, Fields}) ->
    ValuesBinary = argument_list(Fields),
    <<"HDEL ", Key/binary, " ", ValuesBinary/binary>>;
resp_encode({hexists, Key, Field}) ->
    <<"HEXISTS ", Key/binary, " ", Field/binary>>;

resp_encode({hget, Key, Field}) ->
    <<"HGET ", Key/binary, " ", Field/binary>>;
resp_encode({hgetall, Key}) ->
    <<"HGETALL ", Key/binary>>;
resp_encode({hmget, Key, Fields}) ->
    ValuesBinary = argument_list(Fields),
    <<"HMGET ", Key/binary, " ", ValuesBinary/binary>>;
resp_encode({hincrby, Key, Field, Value}) ->
    BinaryVal = integer_to_binary(Value),
    <<"HINCRBY ", Key/binary, " ", Field/binary, " ", BinaryVal/binary>>;
resp_encode({hincrbyfloat, Key, Field, Value}) ->
    BinaryVal = float_to_binary(Value),
    <<"HINCRBYFLOAT ", Key/binary, " ", Field/binary, " ", BinaryVal/binary>>;

resp_encode({hset, Key, Field, Value}) ->
    <<"HSET ", Key/binary, " ", Field/binary, " ", Value/binary>>;
resp_encode({hsetnx, Key, Field, Value}) ->
    <<"HSETNX ", Key/binary, " ", Field/binary, " ", Value/binary>>;
resp_encode({hmset, Key, Field, ValueProplist}) ->
    ValuesBinary = argument_list(ValueProplist),
    <<"HMSET ", Key/binary, " ", Field/binary, " ", ValuesBinary/binary>>;

resp_encode({hkeys, Key}) ->
    <<"HKEYS ", Key/binary>>;
resp_encode({hvals, Key}) ->
    <<"HVALS ", Key/binary>>;
resp_encode({hstrlen, Key, Field}) ->
    <<"HSTRLEN ", Key/binary, " ", Field/binary>>;

%% HyperLogLog Commands
resp_encode({pfadd, Key, Values}) ->
    ValuesBinary = argument_list(Values),
    <<"PFADD ", Key/binary, ValuesBinary/binary>>;
resp_encode({pfcount, Keys}) ->
    KeysBinary = argument_list(Keys),
    <<"PFCOUNT ", KeysBinary/binary>>;
resp_encode({pfmerge, DestKey, SourceKeys}) ->
    SourceKeysBinary = argument_list(SourceKeys),
    <<"PFMERGE ", DestKey/binary, SourceKeysBinary/binary>>;

resp_encode({raw_query, Query}) ->
    Query.

%%--------------------------------------------
%% Helper functions
%%--------------------------------------------
argument_list(List) when is_list(List) ->
    lists:foldl(
        fun
            ({Key, Val}, Acc) ->
                <<Acc/binary, " ", Key/binary, " ", Val/binary>>;
            (Item, Acc) ->
                <<Acc/binary, " ", Item/binary>>
        end, <<>>, List).