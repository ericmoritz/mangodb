-module(mangodb).

-export([start/0]).

start() ->
    application:start(mangodb).
