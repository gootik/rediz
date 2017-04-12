-module(decoder_test).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


decode_error_test() ->
    {{error, <<"Some error message">>}, <<>>} = rediz_decoder:resp_decode(<<"-Some error message\r\n">>),
    {{error, <<"-double dash">>}, <<>>} = rediz_decoder:resp_decode(<<"--double dash\r\n">>),
    {{error, <<"-double dash">>}, <<"+ExtraStuff">>} = rediz_decoder:resp_decode(<<"--double dash\r\n+ExtraStuff">>).

decode_integer_test() ->
    {{ok, 100}, <<>>} = rediz_decoder:resp_decode(<<":100\r\n">>),
    {{ok, -10000}, <<>>} = rediz_decoder:resp_decode(<<":-10000\r\n">>),
    {{ok, 999}, <<"+ExtraStuff">>} = rediz_decoder:resp_decode(<<":999\r\n+ExtraStuff">>).

decode_string_test() ->
    {{ok, <<"100">>}, <<>>} = rediz_decoder:resp_decode(<<"+100\r\n">>),
    {{ok, <<"+OK">>}, <<>>} = rediz_decoder:resp_decode(<<"++OK\r\n">>),
    {{ok, <<"999">>}, <<"+ExtraStuff">>} = rediz_decoder:resp_decode(<<"+999\r\n+ExtraStuff">>).

decode_bulk_string_test() ->
    {{ok, <<"a">>}, <<>>} = rediz_decoder:resp_decode(<<"$1\r\na\r\n">>),
    {buffer, <<"$10\r\n123456\r\n">>} = rediz_decoder:resp_decode(<<"$10\r\n123456\r\n">>),
    {{ok, undefined}, <<"+ExtraStuff">>} = rediz_decoder:resp_decode(<<"$-1\r\n+ExtraStuff">>).