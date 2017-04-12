%%%-------------------------------------------------------------------
%%% @doc rediz_shackle_client is used to manage connections and
%%%      transfer messages from the client to the server. It is also
%%%      responsible for setting up authentication and selecting the
%%%      default database on connection.
%%% @end
%%%-------------------------------------------------------------------
-module(rediz_shackle_client).

-behavior(shackle_client).

-export([
    init/1,
    setup/2,
    handle_request/2,
    handle_data/2,
    terminate/1
]).

-record(state, {
    auth         = no_auth  :: no_auth | binary(),
    db           = 0        :: 0 | pos_integer(),
    buffer       = <<>>     :: binary(),

    requests_in  = 1        :: pos_integer(),
    requests_out = 1        :: pos_integer()
}).

-define(SETUP_TIMEOUT, 1000).

init(#{auth := Auth, db := Db}) ->
    {ok, #state{
        auth = Auth,
        db = Db}}.

setup(Socket, #state{auth = Auth, db = Db} = State) ->
    case auth(Socket, State, Auth) of
        {ok, State} ->
            select_db(Socket, State, Db);
        {error, Reason, State} ->
            {error, Reason, State}
    end.

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
            {ok, [{Requests, Decoded}], State#state{buffer      = Rest,
                                                    requests_in = Requests + 1}}
    end.

terminate(_State) ->
    ok.

select_db(_Socket, State, 0) ->
    {ok, State};
select_db(Socket, State, Db) ->
    DbBinary = integer_to_binary(Db),
    setup_command(Socket, State, <<"SELECT ", DbBinary/binary, "\r\n">>).

auth(_Socket, State, no_auth) ->
    {ok, State};
auth(Socket, State, Auth) ->
    setup_command(Socket, State, <<"AUTH ", Auth/binary, "\r\n">>).

setup_command(Socket, State, Command) ->
    case gen_tcp:send(Socket, Command) of
        ok ->
            case gen_tcp:recv(Socket, 0, ?SETUP_TIMEOUT) of
                {ok, <<"+OK\r\n">>} ->
                    {ok, State};
                {error, Reason} ->
                    {error, Reason, State}
            end;

        {error, Reason} ->
            {error, Reason, State}
    end.