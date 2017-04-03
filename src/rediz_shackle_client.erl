-module(rediz_shackle_client).

-behavior(shackle_client).

-export([
    init/0,
    setup/2,
    handle_request/2,
    handle_data/2,
    terminate/1
]).

-record(state, {
    buffer       = <<>> :: binary,

    requests_in  = 1    :: pos_integer(),
    requests_out = 1    :: pos_integer()
}).

init() ->
    {ok, #state{}}.

setup(_Socket, State) ->
    {ok, State}.

handle_request(Request, #state{requests_out = Requests} = State) ->
    ReqData = rediz_protocol:encode(Request),

    {ok,
     Requests,
     ReqData,
     State#state{requests_out = Requests + 1}}.

handle_data(Data, #state{requests_in = Requests, buffer = Buffer} = State) ->
    {Decoded, Rest} = rediz_protocol:decode(<<Buffer/binary, Data/binary>>),

    case Decoded of
        buffer ->
            {ok, [], State#state{buffer = Rest}};
        _ ->
            {ok, [{Requests, Decoded}], State#state{buffer      = <<>>,
                                                    requests_in = Requests + 1}}
    end.

terminate(_State) ->
    ok.