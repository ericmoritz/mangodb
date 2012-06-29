-module(mangodb_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:start(cowboy),
    {ok, Pid} = mangodb_sup:start_link(),

    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    cowboy:start_listener(my_http_listener, 100,
                          cowboy_tcp_transport, [{port, 27017}],
                          mangodb_protocol, []),
    {ok, Pid}.

stop(_State) ->
    ok.
