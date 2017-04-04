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
resp_encode({hkeys, Key}) ->
    <<"HKEYS ", Key/binary>>;
resp_encode({hget, Key, Field}) ->
    <<"HGET ", Key/binary, " ", Field/binary>>;
resp_encode({hset, Key, Field, Value}) ->
    <<"HSET ", Key/binary, " ", Field/binary, " ", Value/binary>>;
resp_encode({hmget, Key, Fields}) ->
    FieldsBinary = argument_list(Fields),
    <<"HMGET ", Key/binary, " ", FieldsBinary/binary>>;
resp_encode({hgetall, Key}) ->
    <<"HGETALL ", Key/binary>>;

%% HyperLogLog Commands
resp_encode({pfadd, Key, Values}) ->
    ValuesBinary = argument_list(Values),
    <<"PFADD ", Key/binary, ValuesBinary/binary>>;
resp_encode({pfcount, Keys}) ->
    KeysBinary = argument_list(Keys),
    <<"PFCOUNT ", KeysBinary/binary>>;

resp_encode({raw_query, Query}) ->
    Query.

%%--------------------------------------------
%% Helper functions
%%--------------------------------------------
argument_list(List) when is_list(List) ->
    lists:foldl(
        fun(Item, Acc) ->
            <<Acc/binary, " ", Item/binary>>
        end, <<>>, List).