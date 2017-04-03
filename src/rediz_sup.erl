-module(rediz_sup).

-behaviour(supervisor).

-export([
    start_link/0,
    init/1
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SocketOptions = [
        binary,
        {buffer, 65535},
        {packet, raw},
        {send_timeout, 50},
        {send_timeout_close, true}
    ],

    RedisOptions = [
        {ip, "127.0.0.1"},
        {port, 6379},
        {socket_options, SocketOptions},
        {reconnect, true},
        {reconnect_time_max, timer:minutes(2)},
        {reconnect_time_min, timer:minutes(1)}
    ],

    PoolOptions = [
        {backlog_size, 1024},
        {pool_size, 16}
    ],

    ok = shackle_pool:start(rediz, rediz_shackle_client, RedisOptions, PoolOptions),

    {ok, {{one_for_one, 5, 10}, []}}.
