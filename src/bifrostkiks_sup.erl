%%%-------------------------------------------------------------------
%% @doc bifrostkiks top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(bifrostkiks_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    FileProducer = #{id => file_producer,
		     start => {file_producer, start_link, []}},

    Bifrost = #{id => bifrost,
		start => {bifrost, start_link, [ftp_memory_server_producer,
						[{port, 2121}]]}},
    Children = [FileProducer, Bifrost],

    {ok, { {one_for_all, 1, 1}, Children} }.

%%====================================================================
%% Internal functions
%%====================================================================
