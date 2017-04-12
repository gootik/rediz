-module(rediz_sup).

-behaviour(supervisor).

-include("rediz.hrl").

-export([
    start_link/0,
    start_pool/2,
    init/1
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_pool(Name, #{ip := Ip,
                   port := Port,
                   db := _Db,
                   auth := _Auth} = Options) ->

    SocketOptions = [
        binary,
        {buffer, 65535},
        {packet, raw},
        {reuseaddr, true}
    ],

    RedisOptions = [
        {ip, Ip},
        {port, Port},
        {socket_options, SocketOptions},
        {init_options, Options},
        {reconnect, true},
        {reconnect_time_max, 0},
        {reconnect_time_min, 0}
    ],

    PoolSize = maps:get(pool_size, Options, ?DEFAULT_POOL_SIZE),
    PoolOptions = [
        {backlog_size, 1024},
        {pool_size, PoolSize}
    ],

    ok = shackle_pool:start(Name, rediz_shackle_client, RedisOptions, PoolOptions).

init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.
