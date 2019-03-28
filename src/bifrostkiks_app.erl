%%%-------------------------------------------------------------------
%% @doc bifrostkiks public API
%% @end
%%%-------------------------------------------------------------------

-module(bifrostkiks_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    application:ensure_all_started(kiks),
    application:ensure_all_started(bifrost),
    bifrostkiks_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
