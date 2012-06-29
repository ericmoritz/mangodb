-module(mangodb_protocol).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start_link/4, init/4]).
-record(state, {listenerpid, socket, transport, opts, buffer = <<>>}).

start_link(ListenerPid, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
    {ok, Pid}.

init(ListenerPid, Socket, Transport, Opts) ->
    ok = cowboy:accept_ack(ListenerPid),
    State = #state{listenerpid=ListenerPid,
                   socket=Socket, 
                   transport=Transport,
                   opts=Opts},
    wait_line(State).

wait_line(State=#state{transport=Transport, socket=Socket, buffer=Buffer}) ->
    case Transport:recv(Socket, 0, infinity) of
        {ok, Data} ->
            parse_line(State#state{buffer = <<Buffer/binary, Data/binary>>});
        {error, _Reason} ->
            terminate(State)
    end.

parse_line(State=#state{buffer=Buffer}) ->
    case erlang:decode_packet(line, Buffer, []) of
        {ok, Line, Rest} ->
            handle_line(Line, State#state{buffer=Rest});
        {more, _Length} ->
            wait_line(State);
        {error, _Reason} ->
            terminate(State)
    end.

handle_line(Line, State) ->
    {Command, Data} = command(Line),
    handle_command(Command, Data, State).

handle_command(<<"BYE">>, _Data, State) ->
    terminate(State);
handle_command(_Command, Data, State=#state{transport=Transport, socket=Socket}) ->
    ok = case application:get_env(mangodb, durable) of
             {ok, true} ->
                 mangodb_backend:store_durable(Data);
             {ok, false} ->
                 mangodb_backend:store(Data)
         end,
    % TODO eventual consestency
    Transport:send(Socket, ["OK 42\r\n"]),
    wait_line(State).

command(Line) ->
    command(Line, <<>>).

% space
command(<<32:8, Rest/binary>>, Accum) ->
    {Accum, Rest};
% endline
command(<<"\r\n">>, Accum) ->
    {Accum, <<"\r\n">>};
command(<<Char:8, Rest/binary>>, Accum) ->
    command(Rest, <<Accum/binary, Char:8>>).

terminate(#state{transport=Transport, socket=Socket}) ->
    Transport:close(Socket),
    ok.

-ifdef(TEST).

command_test_() ->
    [
     ?_assertEqual({<<"BYE">>, <<"\r\n">>}, command(<<"BYE\r\n">>)),
     ?_assertEqual({<<>>, <<>>}, command(<<>>)),
     ?_assertEqual({<<"PUT">>, <<"foo\r\n">>}, command(<<"PUT foo\r\n">>))
     ].

-endif.
