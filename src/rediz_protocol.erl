%%%-------------------------------------------------------------------
%%% @doc rediz_protocol is used to encode/decode messages to and from
%%%      a Redis server
%%% @end
%%%-------------------------------------------------------------------
-module(rediz_protocol).

-include("rediz.hrl").

-compile(inline).
-compile({inline_size, 512}).


-export([
    encode/1,
    decode/1
]).

encode(Req) ->
    Command = rediz_encoder:resp_encode(Req),

    <<Command/binary, ?RESP_DELIM>>.

decode(Data) ->
    case binary:last(Data) of
        ?RESP_LF_INT ->
            try_decode(Data);
        _ ->
            {buffer, Data}
    end.

try_decode(Data) ->
    try
        rediz_decoder:resp_decode(Data)
    catch _:_ ->
        {buffer, Data}
    end.