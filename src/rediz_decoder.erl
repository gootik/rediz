%%%-------------------------------------------------------------------
%%% @doc rediz_decoder will decode a RESP (REdis Serialization Protocol)
%%%      binary sent from a server.
%%%
%%%      Please look at https://redis.io/topics/protocol for specifics
%%%      on the protocol.
%%% @end
%%%-------------------------------------------------------------------
-module(rediz_decoder).

-include("rediz.hrl").

-export([
    resp_decode/1
]).

-compile(inline).

-spec resp_decode(binary()) -> {rediz_reply(), binary()} | {buffer, binary()}.
%% Error reply
resp_decode(<<?RESP_TYPE_ERROR, Data/binary>>) ->
    [Error, Rest] = binary:split(Data, <<?RESP_DELIM>>),

    {{error, Error}, Rest};

%% Integer reply
resp_decode(<<?RESP_TYPE_INTEGER, Data/binary>>) ->
    [Bin, Rest] = binary:split(Data, <<?RESP_DELIM>>),
    Integer = binary_to_integer(Bin),

    {{ok, Integer}, Rest};

%% String reply
resp_decode(<<?RESP_TYPE_STRING, Data/binary>>) ->
    [String, Rest] = binary:split(Data, <<?RESP_DELIM>>),

    {{ok, String}, Rest};

%% Bulk String reply
resp_decode(<<?RESP_TYPE_BULK_STRING, Rest/binary>> = Data) ->
    [Len, Data2] = binary:split(Rest, <<?RESP_DELIM>>),

    case Len of
        <<?RESP_NULL>> ->
            {{ok, undefined}, Data2};
        _ ->
            IntLen = binary_to_integer(Len),
            case byte_size(Data2) of
                Size when Size < IntLen + 2 ->
                    {buffer, Data};
                _ ->
                    <<String:IntLen/binary, ?RESP_DELIM, Rest2/binary>> = Data2,

                    {{ok, String}, Rest2}
            end
    end;

%% Array reply
resp_decode(<<?RESP_TYPE_ARRAY, ElementData/binary>> = Data) ->
    [NumElements, Elements] = binary:split(ElementData, <<?RESP_DELIM>>),

    case NumElements of
        <<?RESP_NULL>> ->
            {error, bad_command};
        _ ->
            case decode_array(binary_to_integer(NumElements), [], Elements) of
                buffer ->
                    {buffer, Data};
                {MappedArray, Rest} ->
                    {{ok, MappedArray}, Rest}
            end
    end;

%% Unknown. In this case we should buffer and wait for more data.
resp_decode(Data) ->
    {buffer, Data}.

%% Decodes an array reply
decode_array(N, _, <<>>) when N > 0 ->
    buffer;
decode_array(0, Acc, Data) ->
    {Acc, Data};
decode_array(NumElements, Acc, Data) ->
    {{ok, Decoded}, Rest} = resp_decode(Data),
    decode_array(NumElements - 1, [Decoded | Acc], Rest).