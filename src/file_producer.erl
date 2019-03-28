-module(file_producer).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% API
-export([start_link/0,
	 put_file/4]).

%% Behaviour callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

put_file(Fs, Target, FileBytes, FileSize) ->
    io:fwrite("Step 1~n", []),
    gen_server:cast(?MODULE, {put_file, Fs, Target, FileBytes, FileSize}).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

%% @hidden
init([]) ->
    {ok, Producer} = kiks_producer_sup:add_child("data", "ftp_files"),
    {ok, #{producer => Producer}}.

%% @hidden
handle_call(What, _From, State) ->
    {reply, {error, What}, State}.

%% @hidden
handle_cast({put_file, Fs, Target, FileBytes, FileSize}, State) ->
    io:fwrite("Step 2~n", []),
    #{producer := Producer} = State,
    io:fwrite("Step 3~n", []),
    Payload = erlang:term_to_binary({Fs, Target, FileBytes, FileSize}),
    io:fwrite("Step 4~n", []),
    kiks_producer:send(Producer, Payload),
    io:fwrite("Step 5~n", []),
    {noreply, State};
handle_cast(What, State) ->
    io:fwrite("unsupported cast: ~p~n", [What]),
    {noreply, State}.

%% @hidden
handle_info(_What, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
