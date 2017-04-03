-module(rediz_app).

-behaviour(application).

-export([
    start/0, start/2,
    stop/1
]).

start() ->
    application:ensure_all_started(rediz).

start(_, _) ->
    rediz_sup:start_link().

stop(_) ->
    ok.