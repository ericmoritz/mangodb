-module(mangodb_backend).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {device}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, store/1, store_durable/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    % we use a single backend to optimize concurrent access. 
    % A bug reported by jkreps <https://github.com/dcramer/mangodb/pull/25>
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

store(Data) ->
    gen_server:cast(?SERVER, {store, Data}).

store_durable(Data) ->
    gen_server:call(?SERVER, {store, Data}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, Device} = file:open("/dev/null", [write]),
    {ok, #state{device=Device}}.

handle_call({store, Data}, _From, State) ->
    ok = io:write(State#state.device, Data),
    {reply, ok, State}.

handle_cast({store, Data}, State) ->
    io:write(State#state.device, Data),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

